{config, ...}: let
  sources = import ../../../../sources.nix;
in {
  primary-user.extraGroups = ["nix-access-tokens"];
  ids.gids.nix-access-tokens = 500;
  users.groups.nix-access-tokens.gid = config.ids.gids.nix-access-tokens;

  nix = {
    nixPath = ["nixpkgs=${sources.nixpkgs}"];
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
      (import ../../../../overlays/0-unstable-pkgs)
      (import ../../../../overlays/dircolors-solarized)
      (import ../../../../overlays/fzf-pass)
      (import ../../../../overlays/makemkv)
      (import ../../../../overlays/mako)
      (import ../../../../overlays/notify-send)
      (import ../../../../overlays/pass-with-otp)
      (import ../../../../overlays/powerpanel)
      (import ../../../../overlays/sudo-with-insults)
      (import ../../../../pkgs/connect-to-network/overlay.nix)
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  primary-user.home-manager.xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
