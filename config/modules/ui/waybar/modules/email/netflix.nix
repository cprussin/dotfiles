{ callPackage, config }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit config; };
  launcher-apps = callPackage ../../../launcher/apps { inherit config; };
in

mkEmailModule {
  folder = "Netflix";
  script = "${launcher-apps}/gmail netflix";
}
