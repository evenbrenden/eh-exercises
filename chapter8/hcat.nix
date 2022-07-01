{ mkDerivation, base, bytestring, directory, hspec, lib, process, QuickCheck
, quickcheck-instances, text, time }:
mkDerivation {
  pname = "hcat";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring directory process text time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec QuickCheck quickcheck-instances text ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
