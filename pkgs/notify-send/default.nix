let
  sources = import ../../sources.nix;
in
{ nixpkgs ? sources.nixpkgs
, notify-send ? sources.notify-send
}:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix { src = notify-send; })
    ];
  };
in
pkgs.notify-send
