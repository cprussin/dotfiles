{ config, ... }:

let
  dotfiles = import ../../../../default.nix {};

  overlays = "${dotfiles}/overlays";

  load-overlay = overlay:
    import "${overlays}/${overlay}";

  all-overlays =
    builtins.attrNames (builtins.readDir overlays);
in

{
  nixpkgs.config = import ./nixpkgs-config.nix;
  nixpkgs.overlays = map load-overlay all-overlays;

  primary-user.home-manager = {
    nixpkgs.config = config.nixpkgs.config;
    xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  };
}
