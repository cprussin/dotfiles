{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager.home.packages = lib.mkForce (lib.mkAfter [
    pkgs.noto-fonts-emoji
    pkgs.dejavu_fonts
    pkgs.noto-fonts
    pkgs.google-fonts
  ]);
}
