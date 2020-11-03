{ nixpkgs ? (import ../../sources.nix).nixpkgs-unstable
}:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix)
    ];
  };
in
pkgs.home-assistant
