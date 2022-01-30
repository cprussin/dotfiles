let
  sources = import ../../sources.nix;
in
{ nixpkgs ? sources.nixpkgs
, mautrix-wsproxy ? sources.mautrix-wsproxy
}:
let
  pkgs = import nixpkgs {
    overlays = [
      (import ./overlay.nix { src = mautrix-wsproxy; })
    ];
  };
in
pkgs.mautrix-wsproxy
