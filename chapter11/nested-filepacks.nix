with import <nixpkgs> { };

mkShell {
  buildInputs = with pkgs;
    [ (haskellPackages.ghcWithPackages (p: [ p.base64-bytestring ])) ];
}
