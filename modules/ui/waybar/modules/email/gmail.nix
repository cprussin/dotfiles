{ callPackage, launcher, config }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit config; };
in

mkEmailModule {
  folder = "GMail";
  script = "${launcher}/share/apps/gmail";
}
