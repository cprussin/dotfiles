{ pkgs, config, ... }:

let
  stripOverrides = pkgs.callPackage ../../../lib/stripOverrides.nix {};
in

{
  nixpkgs.overlays = [
    (
      _: super: {
        waybar = super.waybar.override {
          pulseSupport = true;
        };
      }
    )
  ];

  home-manager.users.${config.primaryUserName} = _: {
    imports = [
      ./module.nix
    ];

    programs.waybar = {
      enable = true;
      layer = "top";
      position = "bottom";

      styles = {
        common = {
          border = "none";
          border-radius = "0";
          font-family = config.fontTheme.primaryFont.face;
          font-size = "${toString (1.25 * config.fontTheme.primaryFont.size)}px";
          min-height = "35px";
          padding = "0 20px";
          margin = "4px 10px 0 10px";
        };
        frame = {
          color = config.colorTheme.bright;
          border-top = "3px solid ${config.colorTheme.selection}";
          background = config.colorTheme.background;
        };
        pre."@keyframes blink" = {
          to = {
            background-color = config.colorTheme.white;
            color = config.colorTheme.urgent;
          };
        };
      };

      modules = {
        left = map stripOverrides [
          (pkgs.callPackage ./modules/sway/workspaces.nix { inherit config; })
          (pkgs.callPackage ./modules/sway/mode.nix { inherit config; })
          (pkgs.callPackage ./modules/sway/window.nix {})
        ];

        right = map stripOverrides [
          (pkgs.callPackage ./modules/email/gmail.nix { inherit config; })
          (pkgs.callPackage ./modules/email/netflix.nix { inherit config; })
          (pkgs.callPackage ./modules/email/prussin-net.nix { inherit config; })
          (pkgs.callPackage ./modules/vpn.nix { inherit config; })
          (pkgs.callPackage ./modules/mount/secure.nix {})
          (pkgs.callPackage ./modules/mount/boot.nix {})
          (pkgs.callPackage ./modules/bluetooth.nix { inherit config; })
          (pkgs.callPackage ./modules/wifi.nix { inherit config; })
          (pkgs.callPackage ./modules/eth.nix { inherit config; })
          (pkgs.callPackage ./modules/audio.nix { inherit config; })
          (pkgs.callPackage ./modules/mic.nix { inherit config; })
          (pkgs.callPackage ./modules/battery.nix { inherit config; })
          (pkgs.callPackage ./modules/clock.nix {})
          (pkgs.callPackage ./modules/tray.nix {})
        ];
      };
    };
  };
}
