{ config, pkgs, lib, ... }:

{
  home-manager.users.${config.primaryUserName} = {
    programs.direnv.enable = true;
    home.packages = lib.mkForce [ pkgs.direnv ];
  };
}
