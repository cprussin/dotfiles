{ callPackage, launcher, config }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit config; };
in

mkEmailModule {
  folder = "PrussinNet";
  script = "${launcher}/share/apps/email";
}
