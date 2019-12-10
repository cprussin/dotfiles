{ sources ? import ./sources.nix }:

let
  pkgs = import sources.nixpkgs {};
  excludeGit = builtins.filterSource (
    path: type:
      type != "directory" || baseNameOf path != ".git"
  );
in

pkgs.stdenv.mkDerivation {
  name = "dotfiles";
  src = excludeGit ./.;
  phases = [ "unpackPhase" ];
  unpackPhase = "cp -r $src $out";
}
