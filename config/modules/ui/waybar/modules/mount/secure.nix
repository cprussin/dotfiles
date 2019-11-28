{ callPackage }:

let
  mkMountModule = callPackage ./mkMountModule.nix {};
in

mkMountModule {
  path = "/secure";
  icon = "";
}
