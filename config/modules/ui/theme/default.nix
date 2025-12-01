{
  pkgs,
  lib,
  config,
  ...
}: let
  hm-conf = config.primary-user.home-manager;
in {
  colorTheme.name = "solarized/dark";

  home-manager.users.root.colorTheme.name = config.colorTheme.name;

  primary-user.home-manager = {
    home.packages = lib.mkForce (map (theme: theme.package) (
      [hm-conf.cursorTheme hm-conf.iconTheme]
      ++ hm-conf.font.sansSerif
      ++ hm-conf.font.serif
      ++ hm-conf.font.monospace
      ++ hm-conf.font.emoji
    ));

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
      sansSerif = [
        {
          package = pkgs.dejavu_fonts;
          face = "DejaVu Sans";
        }
      ];
      serif = [
        {
          package = pkgs.dejavu_fonts;
          face = "DejaVu Serif";
        }
      ];
      monospace = [
        {
          package = pkgs.nerd-fonts.dejavu-sans-mono;
          face = "DejaVuSansM Nerd Font Mono";
        }
      ];
      emoji = [
        {
          package = pkgs.noto-fonts-color-emoji;
          face = "Noto Color Emoji";
        }
      ];
    };
  };
}
