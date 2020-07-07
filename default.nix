{ sources ? import ./sources.nix }:

let
  pkgs = import sources.nixpkgs {};
in

pkgs.stdenv.mkDerivation {
  name = "dotfiles";
  src = pkgs.lib.cleanSource ./.;
  phases = [ "unpackPhase" ];
  unpackPhase = "cp -r $src $out";
}
