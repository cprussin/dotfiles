{
  pkgs,
  config,
  lib,
  ...
}: let
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;

  terminal = config.primary-user.home-manager.default-terminal.bin;

  modifier = "Mod4";

  expandSet = {
    criteria,
    commands,
  }:
    map (command: {inherit criteria command;}) commands;

  mkCommands = lib.foldl (commands: set: commands ++ (expandSet set)) [];

  laptopOutput = "eDP-1";
in {
  primary-user.home-manager.wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;

    config = {
      inherit modifier;

      menu = "${terminal} --class launcher --title launcher --name launcher ${pkgs.launcher}/bin/launch ${pkgs.sway}/bin/swaymsg exec";

      workspaceAutoBackAndForth = true;
      bars = [];

      output = {
        "${laptopOutput}".pos = lib.mkIf (!config.hardware.video.hidpi.enable) "0,0 scale 1";
        "*".bg = "${./background.png} fill";
      };

      focus = {
        mouseWarping = true;
        forceWrapping = true;
      };

      gaps = {
        inner = 20;
        smartGaps = true;
      };

      window = {
        border = 3;
        hideEdgeBorders = "smart";
        commands = mkCommands [
          {
            criteria.app_id = "launcher";
            commands = [
              "floating enable"
              "sticky enable"
              "resize set 1500 1000"
            ];
          }
          {
            criteria.app_id = "modal";
            commands = [
              "floating enable"
              "sticky enable"
            ];
          }
          {
            criteria.app_id = "pavucontrol";
            commands = ["floating enable"];
          }
          {
            criteria.app_id = "bluetooth";
            commands = ["floating enable"];
          }
          {
            criteria.app_id = "pinentry-qt";
            commands = [
              "floating enable"
              "sticky enable"
            ];
          }
          # TODO remove me when chrome bug
          # https://bugs.chromium.org/p/chromium/issues/detail?id=1338554 is
          # fixed
          {
            criteria.app_id = "^chrome-.*";
            commands = [
              "shortcuts_inhibitor disable"
            ];
          }
          {
            criteria.app_id = "^brave-.*";
            commands = [
              "shortcuts_inhibitor disable"
            ];
          }
        ];
      };

      keybindings = lib.mkOptionDefault {
        "${modifier}+space" = "exec ${config.primary-user.home-manager.wayland.windowManager.sway.config.menu}";
        "${modifier}+Shift+space" = "exec ${pkgs.launcher}/bin/run $(${pkgs.wl-clipboard}/bin/wl-paste)";

        "${modifier}+tab" = "focus mode_toggle";
        "${modifier}+Shift+tab" = "floating toggle";

        "${modifier}+p" = "exec ${launcher-apps.passwords}";
        "${modifier}+Shift+Return" = "exec ${launcher-apps.lock}";

        "XF86AudioRaiseVolume" = "exec ${launcher-apps.volume} up";
        "${modifier}+Page_Up" = "exec ${launcher-apps.volume} up";
        "XF86AudioLowerVolume" = "exec ${launcher-apps.volume} down";
        "${modifier}+Page_Down" = "exec ${launcher-apps.volume} down";
        "XF86AudioMute" = "exec ${launcher-apps.volume} toggle";
        "${modifier}+End" = "exec ${launcher-apps.volume} toggle";
        "XF86AudioMicMute" = "exec ${launcher-apps.mic-volume} toggle";

        "XF86MonBrightnessUp" = "exec ${launcher-apps.brightness} up";
        "XF86Launch6" = "exec ${launcher-apps.brightness} up";
        "XF86MonBrightnessDown" = "exec ${launcher-apps.brightness} down";
        "XF86Launch5" = "exec ${launcher-apps.brightness} down";

        "${modifier}+Shift+a" = "focus child";
        "${modifier}+parenleft" = "workspace 1";
        "${modifier}+parenright" = "workspace 2";
        "${modifier}+braceright" = "workspace 3";
        "${modifier}+plus" = "workspace 4";
        "${modifier}+braceleft" = "workspace 5";
        "${modifier}+bracketright" = "workspace 6";
        "${modifier}+bracketleft" = "workspace 7";
        "${modifier}+exclam" = "workspace 8";
        "${modifier}+equal" = "workspace 9";
        "${modifier}+asterisk" = "workspace 10";

        "${modifier}+Shift+parenleft" = "move container to workspace 1";
        "${modifier}+Shift+parenright" = "move container to workspace 2";
        "${modifier}+Shift+braceright" = "move container to workspace 3";
        "${modifier}+Shift+plus" = "move container to workspace 4";
        "${modifier}+Shift+braceleft" = "move container to workspace 5";
        "${modifier}+Shift+bracketright" = "move container to workspace 6";
        "${modifier}+Shift+bracketleft" = "move container to workspace 7";
        "${modifier}+Shift+exclam" = "move container to workspace 8";
        "${modifier}+Shift+equal" = "move container to workspace 9";
        "${modifier}+Shift+asterisk" = "move container to workspace 10";
      };
    };

    extraConfig = ''
      bindswitch --locked lid:off output ${laptopOutput} enable
      bindswitch --locked lid:on output ${laptopOutput} disable

      bar {
        swaybar_command ${pkgs.waybar}/bin/waybar
      }
    '';
  };

  autologin-graphical-session = {
    enable = true;
    user = config.primary-user.name;
    sessionScript = "${pkgs.sway}/bin/sway";
  };
}
