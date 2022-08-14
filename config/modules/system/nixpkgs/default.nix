{...}: let
  sources = import ../../../../sources.nix;
in {
  nix.nixPath = ["nixpkgs=${sources.nixpkgs}"];
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
  home-manager.useGlobalPkgs = true;
  primary-user.home-manager.xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
}
