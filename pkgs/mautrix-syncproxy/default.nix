let
  sources = import ../../sources.nix;
in
{ nixpkgs ? sources.nixpkgs
, mautrix-syncproxy ? sources.mautrix-syncproxy
}:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix { src = mautrix-syncproxy; })
    ];
  };
in
pkgs.mautrix-syncproxy
