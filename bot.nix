{ mkDerivation, authenticate-oauth, base, bytestring, conduit
, filepath, foldl, lens, mtl, resourcet, stdenv, text, time, turtle
, twitter-conduit, twitter-types-lens, unbounded-delays
}:
mkDerivation {
  pname = "bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    authenticate-oauth base bytestring conduit filepath foldl lens mtl
    resourcet text time turtle twitter-conduit twitter-types-lens
    unbounded-delays
  ];
  homepage = "https://github.com/mt-caret/bot";
  license = stdenv.lib.licenses.bsd3;
}
