{ pkgs, lib, config, ... }:

{
  primary-user.home-manager = {
    home.packages = lib.mkForce [
      config.primary-user.home-manager.cursorTheme.package
      config.primary-user.home-manager.iconTheme.package
      config.primary-user.home-manager.font.package
    ];

    cursorTheme = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };

    font = {
      package = pkgs.dejavu_fonts;
      face = "DejaVu Sans Mono";
      size = 11;
    };
  };
}
