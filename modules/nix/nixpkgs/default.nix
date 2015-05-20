{ ... }:

{
  nixpkgs = {
    config = import ./config.nix;
    overlays = import ./overlays.nix;
  };
  xdg.configFile = {
    "nixpkgs/config.nix".source = ./config.nix;
    "nixpkgs/overlays.nix".source = ./overlays.nix;
  };
}
