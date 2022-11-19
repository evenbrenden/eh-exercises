{
  description = "EH chapter 15 shell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      ghcVersion = "902";
      system = "x86_64-linux";
      hsPkgs = pkgs.haskell.packages.${"ghc" + ghcVersion}.ghcWithPackages (p:
        with p; [
          # Libraries
          base
          process
          # Tools
          cabal-install
          brittany
          haskell-language-server
          implicit-hie
        ]);
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell { buildInputs = [ hsPkgs ]; };
    };
}
