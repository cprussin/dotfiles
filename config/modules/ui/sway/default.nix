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
  primary-user.home-manager = {
    # slurp is required for xdg-desktop-portal-wlr to be able to select an output
    home.packages = lib.mkForce [pkgs.sway pkgs.xdg-desktop-portal-wlr pkgs.slurp];

    wayland.windowManager.sway = {
      enable = true;
      systemd.enable = true;

      config = {
        inherit modifier;

        menu = "${terminal} --class launcher --title launcher --name launcher ${pkgs.launcher}/bin/launch ${pkgs.sway}/bin/swaymsg exec";

        defaultWorkspace = "workspace 1";
        workspaceAutoBackAndForth = true;
        bars = [];

        output."*".bg = "${./background.png} fill";

        input."type:mouse".pointer_accel = "0.5";
        input."type:touchpad" = {
          tap = "enabled";
          tap_button_map = "lrm";
        };

        fonts.size = 11.0;

        focus = {
          mouseWarping = true;
          wrapping = "yes";
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
                "border pixel"
                "floating enable"
                "sticky enable"
                "resize set 1500 1000"
              ];
            }
            {
              criteria.app_id = "modal";
              commands = [
                "border pixel"
                "floating enable"
                "sticky enable"
              ];
            }
            {
              criteria.app_id = "pinentry-qt";
              commands = [
                "border pixel"
                "floating enable"
                "sticky enable"
              ];
            }
            {
              criteria.app_id = "pavucontrol";
              commands = [
                "floating enable"
              ];
            }
            {
              criteria.app_id = "bluetooth";
              commands = [
                "floating enable"
              ];
            }
            {
              criteria.app_id = "org.twosheds.iwgtk";
              commands = [
                "floating enable"
              ];
            }
            {
              criteria.title = "Phantom Wallet";
              commands = [
                "floating enable"
              ];
            }
            {
              criteria.app_id = "^brave-.*";
              commands = [
                "shortcuts_inhibitor disable"
                "inhibit_idle fullscreen"
              ];
            }
            {
              criteria.app_id = "mpv";
              commands = [
                "inhibit_idle fullscreen"
              ];
            }
            {
              criteria.title = "(?:Open|Save) (?:File|Folder|As)";
              commands = [
                "floating enable"
                "resize set width 1030 height 710"
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

    xdg.portal = {
      extraPortals = [pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk];
      configPackages = [pkgs.sway];
      config.sway.default = ["wlr" "gtk"];
    };
  };

  services.displayManager = {
    defaultSession = "sway";
    sessionPackages = [pkgs.sway];
  };
}
