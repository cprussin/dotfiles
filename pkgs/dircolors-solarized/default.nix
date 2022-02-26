let
  sources = import ../../sources.nix;
in
  {
    nixpkgs ? sources.nixpkgs,
    dircolors-solarized ? sources.dircolors-solarized,
  }: let
    pkgs = import nixpkgs {
      overlays = [
        (import ./overlay.nix {src = dircolors-solarized;})
      ];
    };
  in
    pkgs.dircolors-solarized
