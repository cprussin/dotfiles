{ nixpkgs ? (import ../../sources.nix).nixpkgs }:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix)
      (import ../../overlays/zoom-frm)
    ];
  };
in
pkgs.emacsPackages.emacs-rc
