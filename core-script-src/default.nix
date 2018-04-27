{ mkDerivation, aeson, base, bytestring, Cabal, containers, dotenv
, envy, foldl, lens, mtl, regex-compat, servant-auth-server, stdenv
, system-filepath, text, turtle, yaml
}:
mkDerivation {
  pname = "hasm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring Cabal containers dotenv envy foldl lens mtl
    regex-compat servant-auth-server system-filepath text turtle yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring Cabal containers dotenv envy foldl lens mtl
    regex-compat servant-auth-server system-filepath text turtle yaml
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal containers dotenv envy foldl lens mtl
    regex-compat servant-auth-server system-filepath text turtle yaml
  ];
  homepage = "https://github.com/smaccoun/create-haskstar-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
