{ callPackage, config }:

let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit config; };
  launcher-apps = callPackage ../../../launcher/apps { inherit config; };
in

mkEmailModule {
  folder = "GMail";
  script = "${launcher-apps}/gmail";
}
