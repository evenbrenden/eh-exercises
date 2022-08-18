{
  description = "EH chapter 11 shell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      ghcVersion = "902";
      system = "x86_64-linux";
      hsPkgs = pkgs.haskell.packages.${"ghc" + ghcVersion}.ghcWithPackages (p:
        with p; [
          # Libraries
          p.base64-bytestring
          # Tools
          brittany
          haskell-language-server
        ]);
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell { buildInputs = [ hsPkgs ]; };
    };
}
