{ pkgs, config, lib, ... }:

let
  font = config.primary-user.home-manager.font;
  keymap = config.primary-user.home-manager.keymap;
  colors = config.primary-user.home-manager.colorTheme;

  launcher-apps = pkgs.callPackage ../launcher/apps { inherit config; };

  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  notify-send = "${pkgs.notify-send}/bin/notify-send";
  pamixer = "${pkgs.pamixer}/bin/pamixer";

  show-volume = pkgs.writeShellScript "show-volume" ''
    level=$(${pamixer} --get-volume)
    isMuted=$(${pamixer} --get-mute)

    if [ "$isMuted" == "true" ]
    then
      icon=notification-audio-volume-muted
      # color="${colors.highlightBackground}"
    elif [ $level -ge 50 ]
    then
      icon=notification-audio-volume-high
    elif [ $level -ge 25 ]
    then
        icon=notification-audio-volume-medium
    elif [ $level -eq 0 ]
    then
        icon=notification-audio-volume-muted
    else
        icon=notification-audio-volume-low
    fi

    ${notify-send} \
      -R "$XDG_RUNTIME_DIR/volume.msgid" \
      -i $icon \
      -h int:value:$level \
      ""
  '';

  raise-volume = pkgs.writeShellScript "raise-volume" ''
    ${pamixer} --increase 5
    ${show-volume}
  '';

  lower-volume = pkgs.writeShellScript "lower-volume" ''
    ${pamixer} --decrease 5
    ${show-volume}
  '';

  toggle-mute = pkgs.writeShellScript "toggle-mute" ''
    ${pamixer} --toggle-mute
    ${show-volume}
  '';

  show-brightness = pkgs.writeShellScript "show-brightness" ''
    level=$((100 * $(${brightnessctl} get) / $(${brightnessctl} max)))

    if [ $level -eq 100 ]
    then
      icon=notification-display-brightness-full
    elif [ $level -ge 50 ]
    then
      icon=notification-display-brightness-high
    elif [ $level -ge 25 ]
    then
      icon=notification-display-brightness-medium
    elif [ $level -ge 5 ]
    then
      icon=notification-display-brightness-low
    else
      icon=notification-display-brightness-off
    fi

    ${notify-send} \
      -R "$XDG_RUNTIME_DIR/brightness.msgid" \
      -i $icon \
      -h int:value:$level \
      ""
  '';

  raise-brightness = pkgs.writeShellScript "raise-brightness" ''
    ${brightnessctl} set 5%+
    ${show-brightness}
  '';

  lower-brightness = pkgs.writeShellScript "lower-brightness" ''
    ${brightnessctl} set 5%-
    ${show-brightness}
  '';
in

{
  primary-user.home-manager = {
    xdg.configFile."sway/config".text = ''
      set $mod Mod4
      set $left h
      set $down j
      set $up k
      set $right l

      set $background ${colors.background}
      set $foreground ${colors.foreground}
      set $highlightBackground ${colors.highlightBackground}
      set $highlightForeground ${colors.highlightForeground}
      set $selection ${colors.selection}
      set $bright ${colors.bright}
      set $urgent ${colors.urgent}

      set $font pango:${font.face} ${toString font.size}

      set $bgimage ${./background.png}

      set $launch ${pkgs.launcher}/bin/launch
      set $run ${pkgs.launcher}/bin/run
      set $wl-paste ${pkgs.wl-clipboard}/bin/wl-paste
      set $passwords ${launcher-apps.passwords}
      set $waybar ${pkgs.waybar}/bin/waybar
      set $lock ${pkgs.swaylock}/bin/swaylock
      set $idle ${pkgs.swayidle}/bin/swayidle
      set $swaymsg ${pkgs.sway}/bin/swaymsg
      set $mako ${pkgs.mako}/bin/mako
      set $xrdb ${pkgs.xorg.xrdb}/bin/xrdb
      set $systemctl ${pkgs.systemd}/bin/systemctl


      ##
      # Inputs
      ##
      ${if keymap == null then "" else ''
      input * {
        xkb_layout ${keymap.layout}
        xkb_variant ${keymap.variant}
        xkb_options ${keymap.options}
      }
    ''}


      ##
      # Outputs
      ##
      output eDP-1 pos 0,0


      ##
      # Styles
      ##
      output * bg $bgimage fill
      font $font
      default_border pixel 3
      gaps inner 20
      smart_gaps on
      hide_edge_borders --i3 smart_no_gaps
      client.focused $selection $selection $background $bright $selection
      client.focused_inactive $highlightForeground $highlightForeground $highlightBackground $highlightForeground $highlightForeground
      client.unfocused $highlightForeground $highlightBackground $highlightForeground $highlightBackground $highlightBackground
      client.urgent $urgent $urgent $background $urgent $urgent


      ##
      # Window Rules
      ##
      for_window [app_id="pavucontrol"] floating enable
      for_window [app_id="bluetooth"] floating enable
      for_window [app_id="launcher"] {
        floating enable
        sticky enable
        resize set 1500 1000
      }
      for_window [class="Pinentry"] {
        floating enable
        sticky enable
      }
      for_window [app_id="modal"] {
        floating enable
        sticky enable
      }
      for_window [app_id="waybar" floating] {
        move position cursor
        move up 80px
        move left 100px
      }


      ##
      # Bar
      ##
      bar {
        swaybar_command $waybar
      }


      ##
      # Mousebindings
      ##
      mouse_warping container
      floating_modifier $mod normal


      ##
      # Keybindings
      ##
      workspace_auto_back_and_forth yes
      bindsym {
        # Launchers
        $mod+d exec ${config.primary-user.home-manager.default-terminal} --title launcher --name launcher -e '$launch $swaymsg exec'
        $mod+Shift+d exec $run "$($wl-paste)"
        $mod+p exec $passwords
        $mod+Return exec ${config.primary-user.home-manager.default-terminal}
        $mod+Shift+Return exec $lock
        $mod+Shift+q kill
        $mod+Shift+c reload
        XF86AudioRaiseVolume exec ${raise-volume}
        $mod+Page_Up exec ${raise-volume}
        XF86AudioLowerVolume exec ${lower-volume}
        $mod+Page_Down exec ${lower-volume}
        XF86AudioMute exec ${toggle-mute}
        $mod+End exec ${toggle-mute}
        XF86MonBrightnessUp exec ${raise-brightness}
        XF86Launch6 exec ${raise-brightness}
        XF86MonBrightnessDown exec ${lower-brightness}
        XF86Launch5 exec ${lower-brightness}

        # Layout/window management
        $mod+b splith
        $mod+v splitv
        $mod+s layout stacking
        $mod+w layout tabbed
        $mod+e layout toggle split
        $mod+f fullscreen
        $mod+Shift+space floating toggle
        $mod+space focus mode_toggle
        $mod+a focus parent
        $mod+Shift+a focus child

        # Focus management
        $mod+$left focus left
        $mod+$down focus down
        $mod+$up focus up
        $mod+$right focus right
        $mod+Shift+$left move left
        $mod+Shift+$down move down
        $mod+Shift+$up move up
        $mod+Shift+$right move right

        # Workspaces
        $mod+Shift+minus move scratchpad
        $mod+minus scratchpad show
        $mod+parenleft workspace 1
        $mod+parenright workspace 2
        $mod+braceright workspace 3
        $mod+plus workspace 4
        $mod+braceleft workspace 5
        $mod+bracketright workspace 6
        $mod+bracketleft workspace 7
        $mod+exclam workspace 8
        $mod+equal workspace 9
        $mod+asterisk workspace 10
        $mod+Shift+parenleft move container to workspace 1
        $mod+Shift+parenright move container to workspace 2
        $mod+Shift+braceright move container to workspace 3
        $mod+Shift+plus move container to workspace 4
        $mod+Shift+braceleft move container to workspace 5
        $mod+Shift+bracketright move container to workspace 6
        $mod+Shift+bracketleft move container to workspace 7
        $mod+Shift+exclam move container to workspace 8
        $mod+Shift+equal move container to workspace 9
        $mod+Shift+asterisk move container to workspace 10

        # Modes
        $mod+r mode "resize"
      }

      mode "resize" {
        bindsym {
          $left resize shrink width 30px
          $down resize grow height 30px
          $up resize shrink height 30px
          $right resize grow width 30px
          Return mode "default"
          Escape mode "default"
        }
      }

      exec $idle -w \
        timeout 300 $lock \
        timeout 300 '$swaymsg "output * dpms off"' \
          resume '$swaymsg "output * dpms on"' \
        before-sleep $lock \
        lock $lock

      exec $mako
      exec "$systemctl --user import-environment; $systemctl --user start sway-session.target"
    '';

    home.packages = lib.mkForce [
      pkgs.xwayland
      pkgs.libappindicator-gtk3
    ];

    systemd.user.targets.sway-session.Unit = {
      Description = "sway compositor session";
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
  };

  security.pam.services.swaylock.text = "auth include login";

  autologin-graphical-session = {
    enable = true;

    user = config.primary-user.name;

    sessionScript = pkgs.writeShellScript "sway-session-script" ''
      . $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export QT_WAYLAND_FORCE_DPI=physical
      export MOZ_ENABLE_WAYLAND=1

      exec ${pkgs.dbus}/bin/dbus-launch --exit-with-session ${pkgs.sway}/bin/sway
    '';
  };
}
