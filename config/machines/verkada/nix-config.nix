{pkgs, ...}: let
  sources = import ../../../sources.nix;
in {
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.nix;
    nixPath = ["nixpkgs=${sources.nixpkgs}:darwin=${sources.nix-darwin}"];
  };
}
