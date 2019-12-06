{ pkgs, lib, ... }:

{
  primary-user.home-manager = {
    programs.direnv.enable = true;
    home.packages = lib.mkForce [ pkgs.direnv ];
  };
}
