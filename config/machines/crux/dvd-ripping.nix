{
  lib,
  pkgs,
  ...
}: {
  imports = [../../modules/system/devices/cdrom];
  primary-user = {
    home-manager.home.packages = lib.mkForce [
      pkgs.xpra
      pkgs.makemkv
      pkgs.ccextractor
      pkgs.filebot
    ];
  };
}
