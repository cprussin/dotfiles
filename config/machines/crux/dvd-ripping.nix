{
  lib,
  pkgs,
  ...
}: {
  primary-user = {
    home-manager.home.packages = lib.mkForce [
      pkgs.xpra
      pkgs.makemkv
      pkgs.ccextractor
      pkgs.filebot
    ];
  };
}
