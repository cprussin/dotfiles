{ pkgs, ... }:

let
  volume = pkgs.callPackage ./volume.nix {};
in

{
  nixpkgs.overlays = [
    (self: super: { inherit volume; })
  ];

  home.packages = [ pkgs.volume ];
}
