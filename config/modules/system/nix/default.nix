{
  config,
  pkgs,
  ...
}: let
  pkgs-unstable = import config.flake-inputs.nixpkgs-unstable {
    overlays = [];
    config = import ./nixpkgs-config.nix;
    inherit (pkgs) system;
  };
  pkgs-master = import config.flake-inputs.nixpkgs-master {
    overlays = [];
    config = import ./nixpkgs-config.nix;
    inherit (pkgs) system;
  };
  unstable-pkgs-overlay = _: super: {
    inherit (pkgs-unstable) syncthing bitwig-studio;
    inherit (pkgs-master) makemkv;
  };
in {
  primary-user.extraGroups = ["nix-access-tokens"];
  ids.gids.nix-access-tokens = 500;
  users.groups.nix-access-tokens.gid = config.ids.gids.nix-access-tokens;

  nix = {
    channel.enable = false;
    registry.nixpkgs.flake = config.flake-inputs.nixpkgs;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "-d";
    };
    settings = {
      trusted-substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      experimental-features = "flakes nix-command";
      auto-optimise-store = true;
    };
  };

  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = [
      unstable-pkgs-overlay
      (import ../../../../pkgs/dircolors-solarized/overlay.nix {
        src = config.flake-inputs.dircolors-solarized;
      })
      (import "${config.flake-inputs.fzf-pass}/overlay.nix")
      (import ../../../../pkgs/makemkv/overlay.nix)
      (import ../../../../pkgs/mako/overlay.nix)
      (import ../../../../pkgs/notify-send/overlay.nix {
        src = config.flake-inputs.notify-send;
      })
      (import ../../../../pkgs/pass-with-otp/overlay.nix)
      (import ../../../../pkgs/powerpanel/overlay.nix)
      (import ../../../../pkgs/sudo-with-insults/overlay.nix)
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  primary-user.home-manager.xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
