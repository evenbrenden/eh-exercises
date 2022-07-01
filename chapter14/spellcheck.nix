{ mkDerivation, base, containers, hashable, lib, text, vector }:
mkDerivation {
  pname = "spellcheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers hashable text vector ];
  executableHaskellDepends = [ base text ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
