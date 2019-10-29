{ pkgs, config, ... }:

{
  home-manager.users.${config.primaryUserName} = homeManager: {
    home.file.".xmonad/lib" = {
      source = ./lib;
      recursive = true;
      onChange = homeManager.config.home.file.".xmonad/xmonad.hs".onChange;
    };

    xsession = {
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = pkgs.writeText "xmonad.hs" ''
          import NixConfig (NixConfig(NixConfig), Paths(Paths), Font(Font), ColorTheme(ColorTheme))
          import qualified NixConfig as NixConfig
          import WindowManager (start)

          main :: IO ()
          main = start NixConfig
            { NixConfig.paths = Paths
              { NixConfig.xmobar = "${pkgs.haskellPackages.xmobar}/bin/xmobar"
              , NixConfig.xsetroot = "${pkgs.xorg.xsetroot}/bin/xsetroot"
              , NixConfig.launch = "${pkgs.launcher}/bin/launch"
              , NixConfig.xclip = "${pkgs.xclip}/bin/xclip"
              , NixConfig.lockScreen = "${pkgs.lock-screen}/bin/lock-screen"
              , NixConfig.backlight = "${pkgs.backlight}/bin/backlight"
              , NixConfig.volume = "${pkgs.volume}/bin/volume"
              , NixConfig.rofiPass = "${pkgs.rofi-pass}/bin/rofi-pass"
              , NixConfig.terminal = "${config.terminal}"
              }
            , NixConfig.primaryFont = Font
              { NixConfig.face = "${config.fontTheme.primaryFont.face}"
              , NixConfig.size = ${toString config.fontTheme.primaryFont.size}
              }
            , NixConfig.colorTheme = ColorTheme
              { NixConfig.background = "${config.colorTheme.background}"
              , NixConfig.foreground = "${config.colorTheme.foreground}"
              , NixConfig.highlightBackground = "${config.colorTheme.highlightBackground}"
              , NixConfig.highlightForeground = "${config.colorTheme.highlightForeground}"
              , NixConfig.secondaryContent = "${config.colorTheme.secondaryContent}"
              , NixConfig.selection = "${config.colorTheme.selection}"
              , NixConfig.bright = "${config.colorTheme.bright}"
              , NixConfig.urgent = "${config.colorTheme.urgent}"
              , NixConfig.warn = "${config.colorTheme.warn}"
              , NixConfig.black = "${config.colorTheme.black}"
              , NixConfig.white = "${config.colorTheme.white}"
              , NixConfig.grey = "${config.colorTheme.grey}"
              , NixConfig.lightGrey = "${config.colorTheme.lightGrey}"
              , NixConfig.red = "${config.colorTheme.red}"
              , NixConfig.lightRed = "${config.colorTheme.lightRed}"
              , NixConfig.green = "${config.colorTheme.green}"
              , NixConfig.lightGreen = "${config.colorTheme.lightGreen}"
              , NixConfig.yellow = "${config.colorTheme.yellow}"
              , NixConfig.lightYellow = "${config.colorTheme.lightYellow}"
              , NixConfig.blue = "${config.colorTheme.blue}"
              , NixConfig.lightBlue = "${config.colorTheme.lightBlue}"
              , NixConfig.purple = "${config.colorTheme.purple}"
              , NixConfig.lightPurple = "${config.colorTheme.lightPurple}"
              , NixConfig.cyan = "${config.colorTheme.cyan}"
              , NixConfig.lightCyan = "${config.colorTheme.lightCyan}"
              }
            }
        '';
      };
    };
  };
}
