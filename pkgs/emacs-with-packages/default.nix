{ nixpkgs ? (import ../../sources.nix).nixpkgs
}:

let
  pkgs = import nixpkgs {
    overlays = [
      (import ../zoom-frm/overlay.nix)
      (import ./overlay.nix)
    ];
  };
in

pkgs.emacs
