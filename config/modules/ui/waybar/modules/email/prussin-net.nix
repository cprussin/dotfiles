{ callPackage, config, colors }:
let
  mkEmailModule = callPackage ./mkEmailModule.nix { inherit colors; };
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;
in
mkEmailModule {
  folder = "PrussinNet";
  script = launcher-apps.email;
}
