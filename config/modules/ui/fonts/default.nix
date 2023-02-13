{
  pkgs,
  lib,
  ...
}: {
  nixpkgs.overlays = [
    (import ../../../../pkgs/google-fonts-without-noto-emoji/overlay.nix)
  ];

  primary-user.home-manager.home.packages = lib.mkForce (lib.mkAfter [
    pkgs.noto-fonts-emoji
    pkgs.dejavu_fonts
    pkgs.google-fonts
  ]);
}
