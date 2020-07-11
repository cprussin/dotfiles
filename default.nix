{ nixpkgs ? (import ./sources.nix).nixpkgs }:

let
  pkgs = import nixpkgs {};
in

pkgs.stdenv.mkDerivation {
  name = "dotfiles";
  src = pkgs.lib.cleanSource ./.;
  phases = [ "unpackPhase" ];
  unpackPhase = "cp -r $src $out";
}
