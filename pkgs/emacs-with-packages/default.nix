let
  sources = import ../../sources.nix;
in
{ nixpkgs ? sources.nixpkgs
, zoom-frm ? sources.zoom-frm
}:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ../zoom-frm/overlay.nix { src = zoom-frm; })
      (import ./overlay.nix)
    ];
  };
in
pkgs.emacs
