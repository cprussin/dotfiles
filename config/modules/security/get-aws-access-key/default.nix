{ pkgs, lib, ... }:

{
  primary-user.home-manager.home.packages = lib.mkForce [ pkgs.get-aws-access-key ];
}
