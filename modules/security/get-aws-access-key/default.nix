{ pkgs, config, lib, ... }:

{
  home-manager.users.${config.primaryUserName}.home.packages = lib.mkForce [ pkgs.get-aws-access-key ];
}
