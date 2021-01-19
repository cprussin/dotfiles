{ callPackage, colors, config }:
let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit colors; };
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;
in
mkEmailModule {
  folder = "GMail";
  script = launcher-apps.gmail;
}
