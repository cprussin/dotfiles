{ callPackage, launcher, config }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit config; };
in

mkEmailModule {
  folder = "Netflix";
  script = "${launcher}/share/apps/gmail netflix";
}
