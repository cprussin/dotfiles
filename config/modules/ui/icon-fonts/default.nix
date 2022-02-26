{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager.home.packages = lib.mkForce [
    pkgs.font-awesome_5
    pkgs.noto-fonts
  ];
}
