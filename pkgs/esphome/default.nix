{ nixpkgs ? (import ../../sources.nix).nixpkgs
}:

let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix { inherit nixpkgs; })
    ];
  };
in

pkgs.esphome
