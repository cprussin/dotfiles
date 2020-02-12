{ callPackage }:

let
  sources = import ../../sources.nix;
in

(callPackage sources.nix-linter {}).nix-linter
