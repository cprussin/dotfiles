{ pkgs, config, lib, ... }:

{
  home-manager.users.${config.primary-user.name}.home.packages = lib.mkForce [ pkgs.get-aws-access-key ];
}
