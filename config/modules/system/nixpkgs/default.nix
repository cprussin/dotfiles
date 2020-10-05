{ ... }:

let
  sources = import ../../../../sources.nix;
in

{
  nix.nixPath = [ "nixpkgs=${sources.nixpkgs}" ];
  nixpkgs.config = import ./nixpkgs-config.nix;
  home-manager.useGlobalPkgs = true;
  primary-user.home-manager.xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
