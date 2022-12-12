{
  config,
  pkgs,
  ...
}: let
  sources = import ../../../../sources.nix;
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
in {
  deployment.keys.flox-access-token = {
    keyCommand = passwords.getNixAccessToken "Connor/Infrastructure/access-tokens/github.com-flox";
    destDir = "/secrets";
    group = "nix-access-tokens";
    permissions = "0660";
  };

  primary-user.extraGroups = ["nix-access-tokens"];
  ids.gids.nix-access-tokens = 500;
  users.groups.nix-access-tokens.gid = config.ids.gids.nix-access-tokens;

  nix = {
    nixPath = ["nixpkgs=${sources.nixpkgs}"];
    gc = {
      automatic = true;
      dates = "weekly";
    };
    settings = {
      trusted-substituters = [
        "https://cache.floxdev.com?trusted=1"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "flox-store-public-0:8c/B+kjIaQ+BloCmNkRUKwaVPFWkriSAd0JJvuDu4F0="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      experimental-features = "flakes nix-command";
      auto-optimise-store = true;
    };
    extraOptions = "!include ${config.deployment.keys.flox-access-token.path}";
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
