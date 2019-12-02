{ config, pkgs, lib, ... }:

{
  home-manager.users.${config.primary-user.name} = {
    programs.direnv.enable = true;
    home.packages = lib.mkForce [ pkgs.direnv ];
  };
}
