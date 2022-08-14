{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager.home.packages = lib.mkForce (lib.mkAfter [
    pkgs.font-awesome_5
    pkgs.noto-fonts
    pkgs.google-fonts
  ]);
}
