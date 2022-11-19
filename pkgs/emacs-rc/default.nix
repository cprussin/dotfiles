{
  sources ? import ../../sources.nix,
  nixpkgs ? sources.nixpkgs,
  zoom-frm ? sources.zoom-frm,
  emacs-overlay ? sources.emacs-overlay,
}: let
  pkgs = import nixpkgs {
    overlays = [
      (import emacs-overlay)
      (import ./overlay.nix)
      (import ../emojione-png/overlay.nix)
      (import ../zoom-frm/overlay.nix {src = zoom-frm;})
    ];
  };
in
  pkgs.emacsPackages.emacs-rc
