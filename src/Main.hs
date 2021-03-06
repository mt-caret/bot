{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where


import Prelude
    (IO
    , Maybe(..)
    , Integer
    , Show
    , FilePath
    , String
    , show
    , length
    , print
    , putStrLn
    , undefined
    , (.)
    , ($)
    , (<$>)
    , (==)
    , (/=)
    , (++)
    , (*)
    , (^)
    )
import Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as P
import Web.Twitter.Types.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as S8
import Control.Monad.IO.Class
import Control.Lens
import Data.Time
import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Except as E
import System.Environment
import Data.Monoid ((<>))
import Data.Maybe
import qualified Turtle as TT
import qualified Turtle.Format as TF
import qualified Control.Foldl as Fold
import System.FilePath


getTWInfo :: IO TWInfo
getTWInfo = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_TOKEN"
    accessSecret <- getEnv' "OAUTH_TOKEN_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return $ setCredential oauth cred def
  where
    getEnv' str = S8.pack <$> (getEnv str)


getManager :: IO Manager
getManager = newManager tlsManagerSettings

reply :: T.Text -> Integer -> APIRequest StatusesUpdate Status
reply text id = update text & P.inReplyToStatusId ?~ id

replyWithMedia :: T.Text -> Integer -> FilePath -> APIRequest StatusesUpdateWithMedia Status
replyWithMedia text id fp =
    updateWithMedia text (MediaFromFile fp) & P.inReplyToStatusId ?~ id

escape :: T.Text -> T.Text -> T.Text
escape sn = T.strip . T.replace "@" "" . T.replace ("@" <> sn) " "
-- replacing with " " to prevent stuff like @al@bobice -> @alice


downloadFromURL :: [T.Text] -> IO ()
downloadFromURL urls = TT.sh $ do
    TT.rmtree "/tmp/bot"
    TT.mktree "/tmp/bot"
    TT.cd "/tmp/bot"
    forM_ urls $ \url -> TT.inproc "wget" [url] TT.empty
    TT.cd "/tmp"


extractMediaURLs :: Status -> [T.Text]
extractMediaURLs st =
    let
        getURL med = med ^. entityBody . meMediaURLHttps
        extract ents = ents ^. enMedia ^.to (fmap getURL)
    in
        fromMaybe [] $ st ^. statusEntities ^.to (fmap extract)


toString :: Status -> T.Text
toString st =
    (T.pack . show $ st ^. statusId)
        <> ": "
        <> st ^. statusUser . userScreenName
        <> ": "
        <> st ^. statusText

conv :: TT.FilePath -> String
conv fp = T.unpack (TF.format TF.fp fp)

sink :: TWInfo -> Manager -> T.Text -> Consumer Status (ResourceT IO) ()
sink twInfo mgr sn =
    CL.mapM_ $ \st ->
      let
        processStatus = do
            let id = st ^. statusId
            downloadFromURL . extractMediaURLs $ st
            fps <- TT.fold (TT.ls "/tmp/bot") Fold.list
            putStrLn . show $ fps
            case conv <$> fps of
                [] -> return ()
                fp:_ -> do
                    putStrLn $ "filepath: " ++ fp
                    TT.sh $ TT.cd "~/bot"
                    (code, out) <- TT.shellStrict ("python src/eval.py" <> (T.pack fp)) TT.empty
                    let content
                          = ".@"
                          <> st ^. statusUser . userScreenName
                          <> " "
                          <> out
                    res <- call twInfo mgr $ replyWithMedia content id $ fp ++ (takeExtension fp)
--                    res <- call twInfo mgr $ replyWithMedia content id fp
--                    res <- call twInfo mgr $ replyWithMedia content id fp
                    T.putStrLn . toString $ st
                    T.putStrLn $ "> " <> toString res
      in
        liftIO $ processStatus `catchError` errorHandler

getOwnScreenName :: TWInfo -> Manager -> IO T.Text
getOwnScreenName twInfo mgr = do
    user <- call twInfo mgr accountVerifyCredentials
    return $ user ^. userScreenName


extractStatuses :: StreamingAPI -> Maybe Status
extractStatuses (SStatus s) = Just s
extractStatuses _ = Nothing


errorHandler :: Show e => e -> IO ()
errorHandler e = putStrLn $ "ERROR> " ++ show e


streamTweets :: TWInfo -> Manager -> T.Text -> IO ()
streamTweets twInfo mgr sn =
    E.catchError streamTweets' errorHandler
  where
    streamTweets' = runResourceT $ do
        rsrc <- stream twInfo mgr userstream
        rsrc
            $$+- CL.mapMaybe extractStatuses
            .| CL.filter (\st -> sn /= st ^. statusUser . userScreenName)
            .| sink twInfo mgr sn

main :: IO ()
main = do
    twInfo <- getTWInfo
    mgr <- getManager
    sn <- getOwnScreenName twInfo mgr
    T.putStrLn $ "I'm " <> sn
    forM_ [0..] $ \n -> do
        streamTweets twInfo mgr sn
        putStrLn $ "ERROR> Something terrible happened. (" ++ (show n) ++ ")"
        delay (2^n * 1000 * 1000) -- exponential backoff


