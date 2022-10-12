{
  sources ? import ../../sources.nix,
  nixpkgs ? sources.nixpkgs,
  zoom-frm ? sources.zoom-frm,
}: let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix)
      (import ../zoom-frm/overlay.nix {src = zoom-frm;})
    ];
  };
in
  pkgs.emacsPackages.emacs-rc
