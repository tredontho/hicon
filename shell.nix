let
  sources = import ./nix/sources.nix;
in
  {pkgs ? import sources.nixpkgs {} }:
  pkgs.mkShell {
    buildInputs = with pkgs; [
      sources.niv
      ghc
      cabal-install
      ghcid
      hlint
      ormolu
      zlib
      haskellPackages.ghcide
    ];
  }
