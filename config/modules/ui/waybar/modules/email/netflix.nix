{ callPackage, config, colors }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit colors; };
  launcher-apps = callPackage ../../../launcher/apps { inherit config; };
in

mkEmailModule {
  folder = "Netflix";
  script = "${launcher-apps}/gmail netflix";
}
