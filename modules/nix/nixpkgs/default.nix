{ config, ... }:

{
  nixpkgs.config.allowUnfree = true;

  home-manager.users.${config.primaryUserName}.nixpkgs = {
    overlays = config.nixpkgs.overlays;
    config = config.nixpkgs.config;
  };
}
