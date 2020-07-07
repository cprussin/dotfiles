{ lib, ... }:

let
  sources = import ../../../../sources.nix;
  dotfiles = import ../../../../default.nix {};
in

{
  nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${v}") {
    nixpkgs = sources.nixpkgs;
    nixpkgs-overlays = "${dotfiles}/overlays";
  };
}
