{
  nixpkgs ? (import ../../sources.nix).nixpkgs,
  aptIndexes,
}: let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix {inherit aptIndexes;})
    ];
  };
in
  pkgs.claude-desktop
