{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  gmail-new-mail-counter-overlay = final: prev: {
    gmail-new-mail-counter = final.writeShellScriptBin "gmail_new_mail_counter" ''
      set -a
      . ${config.deployment.keys.gmail-new-mail-counter-env.path}
      set +a
      exec ${prev.gmail-new-mail-counter}/bin/gmail_new_mail_counter "$@"
    '';
  };
  get-mail = indicatorName: address:
    pkgs.writeShellScript "get-mail" ''
      ${pkgs.gmail-new-mail-counter}/bin/gmail_new_mail_counter \
        --format '{{#if (gt total 0)}}{"text":"✉ ${indicatorName} {{ unread }} / {{ total }}"{{#if (gt unread 0) }},"class":"unread"{{/if}}}{{/if}}' \
        --auth-format '{"text":"✉ ${indicatorName} "}' \
        ${address}
    '';
in {
  nixpkgs.overlays = [
    config.flake-inputs.gmail-new-mail-counter.overlays.default
    gmail-new-mail-counter-overlay
  ];

  deployment.keys.gmail-new-mail-counter-env = {
    user = config.primary-user.name;
    group = "users";
    destDir = "/secrets";
    keyCommand = passwords.getGmailNewMailCounterEnvFile "Connor/Infrastructure/gmail-new-mail-counter";
  };

  primary-user.home-manager.programs.waybar = {
    enable = true;

    settings = [
      {
        layer = "top";
        position = "top";

        modules-left = [
          "tray"
          "sway/workspaces"
          "sway/mode"
          "custom/email-connor-prussin-net"
          "custom/email-connor-dourolabs-xyz"
        ];
        modules-center = ["clock"];
        modules-right = [
          "group/volume-group"
          "group/backlight-group"
          "idle_inhibitor"
          "bluetooth"
          "network#wireless"
          "battery"
        ];

        tray.spacing = 10;

        "sway/workspaces" = {};

        "sway/mode".format = "{}";

        clock = {
          format = "{:%A %Y-%m-%d %H:%M:%S}";
          interval = 1;
          tooltip-format = "<tt>{calendar}</tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            format = {
              months = "<span color='lightgray'><b>{}</b></span>";
              days = "<span color='gray'>{}</span>";
              weeks = "<span color='darkgray'>W{}</span>";
              weekdays = "<span color='cyan'><b>{}</b></span>";
              today = "<span background='yellow' color='black'><b>{}</b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };

        "group/volume-group" = {
          modules = ["wireplumber" "pulseaudio/slider#sink"];
          orientation = "inherit";
          drawer = {};
        };
        wireplumber = {
          format = "";
          states = {
            low = 1;
            medium = 30;
            high = 60;
          };
          on-click = "${pkgs.pavucontrol}/bin/pavucontrol -t 3";
          on-click-middle = "${pkgs.pamixer}/bin/pamixer --toggle-mute";
          tooltip-format = "{node_name} [{volume}%]";
        };
        "pulseaudio/slider#sink" = {
          target = "sink";
        };

        "group/backlight-group" = {
          modules = ["backlight" "backlight/slider"];
          orientation = "inherit";
          drawer = {};
        };
        backlight = {
          format = "";
          tooltip-format = "{percent}%";
          states = {
            medium = 40;
            bright = 80;
          };
        };
        "backlight/slider" = {};

        idle_inhibitor = {
          format = " ";
          tooltip-format-activated = "Presentation Mode On";
          tooltip-format-deactivated = "Presentation Mode Off";
        };

        bluetooth = {
          format = " ";
          tooltip = true;
          tooltip-format-disabled = "Bluetooth Disabled";
          tooltip-format-off = "Bluetooth Off";
          tooltip-format-on = "No Devices Connected";
          tooltip-format-connected = "Connected to {device_enumerate}";
          tooltip-format-enumerate-connected = "{device_alias}";
          on-click = config.primary-user.home-manager.programs.launcher.apps.bluetooth;
        };

        "network#wireless" = {
          format = "{icon}";
          format-disconnected = "󰤭";
          format-icons = ["󰤟" "󰤢" "󰤥" "󰤨"];
          interface = "wlan0";
          interval = "5";
          on-click = "${pkgs.iwgtk}/bin/iwgtk";
          tooltip-format = "{icon} {ifname}: ({essid}) {ipaddr}/{cidr} {frequency}mhz [{signalStrength}%]";
        };
        battery = {
          format = "{icon} {capacity}%";
          format-alt = "{icon} {time}";
          format-charging = "{icon} 󰚥 {capacity}%";
          format-icons = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
          format-plugged = "󰚥 {capacity}%";
          states = {
            critical = 5;
            warning = 15;
          };
        };

        "custom/email-connor-dourolabs-xyz" = {
          exec = get-mail "Douro Labs" "connor@dourolabs.xyz";
          interval = 20;
          on-click = "${pkgs.launcher}/bin/browse https://mail.google.com?authuser=connor@dourolabs.xyz";
          return-type = "json";
        };
        "custom/email-connor-prussin-net" = {
          exec = get-mail "PrussinNet" "connor@prussin.net";
          interval = 20;
          on-click = "${pkgs.launcher}/bin/browse https://mail.google.com?authuser=connor@prussin.net";
          return-type = "json";
        };
      }
    ];

    style = ''
      window#waybar {
        background: transparent;
        color: lightgray;
        font-size: 0.875rem;
      }

      .modules-left,
      .modules-center,
      .modules-right {
        padding: 0.5rem 1rem;
      }

      .module,
      #backlight-group,
      #volume-group {
        padding: 0;
        margin-top: 0;
        margin-bottom: 0;
        margin-left: 1rem;
        margin-right: 1rem;
      }

      #backlight-group .module,
      #volume-group .module {
        margin-left: 0;
        margin-right: 0;
      }

      trough {
        min-height: 0.25rem;
        min-width: 4rem;
        background: #333;
        border: 0;
        margin-left: 1rem;
        margin-right: 1rem;
        box-shadow: 1px 1px 2px black;
      }
      slider {
        min-width: 0.5rem;
        min-height: 0.5rem;
        background-image: none;
        background-color: lightgrey;
        border: none;
        box-shadow: none;
        padding: 0;
        margin: -0.5rem;
      }
      highlight {
        background: darkgrey;
        border: none;
      }

      #backlight,
      #wireplumber,
      #bluetooth,
      #idle_inhibitor {
        background-position: center;
        background-repeat: no-repeat;
        background-size: 1.25rem 1.25rem;
        min-width: 1.25rem;
      }

      #backlight,
      #wireplumber {
        margin: 0;
      }

      #workspaces button {
        color: lightgray;
        padding: 0;
        margin: 0;
        min-width: 1.5rem;
        min-height: 1.5rem;
        border-radius: 1.5rem;
        margin-left: 0.25rem;
        margin-right: 0.25rem;
        border: none;
        box-shadow: inherit;
        text-shadow: inherit;
        border: 1px solid transparent;
      }
      #workspaces button:hover {
        border-color: gray;
        background: transparent;
        color: lightgray;
      }
      #workspaces button:first-child {
        margin-left: 0;
      }
      #workspaces button:last-child {
        margin-right: 0;
      }
      #workspaces button.focused {
        background: lightgray;
        color: black;
        font-weight: bold;
      }
      #workspaces button.focused:hover {
        border-color: transparent;
      }

      #mode {
        background: lightgray;
        color: black;
        border-radius: 1.5rem;
        padding-left: 1rem;
        padding-right: 1rem;
        font-weight: bold;
      }

      #wireplumber {
        background-image: url("${./icons/speaker-slash.svg}");
      }
      #wireplumber.low {
        background-image: url("${./icons/speaker-none.svg}");
      }
      #wireplumber.medium {
        background-image: url("${./icons/speaker-low.svg}");
      }
      #wireplumber.high {
        background-image: url("${./icons/speaker-high.svg}");
      }
      #wireplumber.muted {
        background-image: url("${./icons/speaker-slash.svg}");
      }
      #wireplumber.source {
        background-image: url("${./icons/microphone-slash.svg}");
      }
      #wireplumber.source.on {
        background-image: url("${./icons/microphone.svg}");
      }
      #wireplumber.source.muted {
        background-image: url("${./icons/microphone-slash.svg}");
      }

      #backlight {
        background-image: url("${./icons/moon.svg}");
      }
      #backlight.medium {
        background-image: url("${./icons/sun-dim.svg}");
      }
      #backlight.bright {
        background-image: url("${./icons/sun.svg}");
      }

      #bluetooth,
      #bluetooth.on {
        background-image: url("${./icons/bluetooth.svg}");
      }
      #bluetooth.connected {
        background-image: url("${./icons/bluetooth-connected.svg}");
      }
      #bluetooth.disabled,
      #bluetooth.no-controller,
      #bluetooth.off {
        background-image: url("${./icons/bluetooth-slash.svg}");
      }

      #idle_inhibitor.activated {
        background-image: url("${./icons/eye.svg}");
      }
      #idle_inhibitor.deactivated {
        background-image: url("${./icons/eye-slash.svg}");
      }
    '';
  };
}
