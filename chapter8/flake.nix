{
  description = "hcat";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        index-state = "2022-08-18T00:00:00Z";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hcatProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc902";
              shell = {
                tools = { cabal = { inherit index-state; }; };
                # https://github.com/input-output-hk/haskell.nix/issues/1587
                # buildInputs = with pkgs.haskellPackages; [
                #   brittany
                #   haskell-language-server
                #   implicit-hie
                # ];
              };
              inherit index-state;
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hcatProject.flake { };
      in flake // { defaultPackage = flake.packages."hcat:exe:hcat"; });
}
