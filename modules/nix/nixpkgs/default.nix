{ config, ... }:

{
  nixpkgs.config = import ./nixpkgs-config.nix;

  home-manager.users.${config.primaryUserName} = {
    nixpkgs = {
      overlays = config.nixpkgs.overlays;
      config = config.nixpkgs.config;
    };

    xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  };
}
