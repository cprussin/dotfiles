{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  stripOverrides = pkgs.callPackage ../../../../lib/stripOverrides.nix {};
  colors = config.primary-user.home-manager.colorTheme;
  gmail-new-mail-counter-overlay = final: prev: {
    gmail-new-mail-counter = final.writeShellScriptBin "gmail_new_mail_counter" ''
      set -a
      . ${config.deployment.keys.gmail-new-mail-counter-env.path}
      set +a
      exec ${prev.gmail-new-mail-counter}/bin/gmail_new_mail_counter "$@"
    '';
  };
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

  primary-user.home-manager.programs.waybar-custom = {
    enable = true;
    layer = "top";
    position = "bottom";

    styles = {
      common = {
        border = "none";
        border-radius = "0";
        min-height = "35px";
        padding = "0 20px";
        margin = "4px 10px 0 10px";
        font-family = "monospace";
      };
      frame = {
        border-top = "3px solid ${colors.selection}";
      };
      pre."@keyframes blink" = {
        to = {
          background-color = colors.white;
          color = colors.urgent;
        };
      };
    };

    modules = {
      left = map stripOverrides [
        (pkgs.callPackage ./modules/sway/workspaces.nix {inherit colors;})
        (pkgs.callPackage ./modules/sway/mode.nix {inherit colors;})
        (pkgs.callPackage ./modules/sway/window.nix {})
      ];

      right = map stripOverrides (
        builtins.filter (m: m != null) [
          (pkgs.callPackage ./modules/presentation.nix {inherit colors;})
          (pkgs.callPackage ./modules/email.nix {
            inherit colors;
            address = "connor@prussin.net";
            indicatorName = "PrussinNet";
          })
          (pkgs.callPackage ./modules/email.nix {
            inherit colors;
            address = "connor@dourolabs.xyz";
            indicatorName = "Pyth";
          })
          (pkgs.callPackage ./modules/email.nix {
            inherit colors;
            address = "cprussin@getsharpe.io";
            indicatorName = "GetSharpe";
          })
          (pkgs.callPackage ./modules/bluetooth.nix {inherit config colors;})
          (
            if config.interfaces.wifi == null
            then null
            else pkgs.callPackage ./modules/wifi.nix {inherit config colors;}
          )
          (
            if config.interfaces.eth == null
            then null
            else pkgs.callPackage ./modules/eth.nix {inherit config colors;}
          )
          (pkgs.callPackage ./modules/audio.nix {inherit colors;})
          (pkgs.callPackage ./modules/mic.nix {inherit colors;})
          (pkgs.callPackage ./modules/battery.nix {inherit colors;})
          (pkgs.callPackage ./modules/clock.nix {})
          (pkgs.callPackage ./modules/tray.nix {})
        ]
      );
    };
  };
}
