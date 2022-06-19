# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#getting-started-with-flakes
{
  description = "hcat";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  nixConfig.bash-prompt = "[hcat]$ ";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hcatProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = { cabal = { }; };
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
