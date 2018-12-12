{ pkgs, ... }:

let
  setup-monitors = pkgs.callPackage ./setup-monitors.nix {};
in

{
  nixpkgs.overlays = [
    (self: super: { inherit setup-monitors; })
  ];
}
