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
          import NixConfig (NixConfig(NixConfig), Paths(Paths), Font(Font))
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
              { NixConfig.face = "${config.primaryFont.face}"
              , NixConfig.size = ${toString config.primaryFont.size}
              }
            }
        '';
      };
    };
  };
}
