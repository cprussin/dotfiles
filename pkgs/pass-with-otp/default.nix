{nixpkgs ? (import ../../sources.nix).nixpkgs}: let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix)
    ];
  };
in
  pkgs.pass
