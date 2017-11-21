{ mkDerivation, authenticate-oauth, base, bytestring, conduit
, foldl, lens, resourcet, stdenv, text, time, turtle
, twitter-conduit, twitter-types-lens
}:
mkDerivation {
  pname = "bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    authenticate-oauth base bytestring conduit foldl lens resourcet
    text time turtle twitter-conduit twitter-types-lens
  ];
  homepage = "https://github.com/mt-caret/bot";
  license = stdenv.lib.licenses.bsd3;
}
