# Not using haskell.nix because I don't know how to set it up with profiling
{
  description = "EH chapter 14 shell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      ghcVersion = "902";
      system = "x86_64-linux";
      hsPkgs = pkgs.haskell.packages.${"ghc" + ghcVersion}.ghcWithPackages (p:
        with p; [
          # Libraries
          base
          containers
          hashable
          text
          vector
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
