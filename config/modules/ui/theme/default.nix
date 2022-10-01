{
  pkgs,
  lib,
  config,
  ...
}: {
  colorTheme.name = "solarized/dark";

  home-manager.users.root.colorTheme.name = config.colorTheme.name;

  primary-user.home-manager = {
    home.packages = lib.mkForce [
      config.primary-user.home-manager.cursorTheme.package
      config.primary-user.home-manager.iconTheme.package
      config.primary-user.home-manager.font.package
    ];

    colorTheme.name = config.colorTheme.name;

    cursorTheme = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };

    font = {
      package = pkgs.nerdfonts.override {fonts = ["DejaVuSansMono"];};
      face = "DejaVu Sans Mono Nerd Font";
      size = 11.0;
    };
  };
}
