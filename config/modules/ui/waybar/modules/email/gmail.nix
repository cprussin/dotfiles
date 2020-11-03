{ callPackage, colors, config }:
let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit colors; };
  launcher-apps = callPackage ../../../launcher/apps { inherit config; };
in
mkEmailModule {
  folder = "GMail";
  script = launcher-apps.gmail;
}
