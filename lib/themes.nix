{ callPackage }:

let
  stripOverrides = callPackage ./stripOverrides.nix {};

  theme = type: name:
    stripOverrides (
      callPackage (../modules/ui/theme + "/${type}/${name}.nix") {}
    );
in

{
  font = theme "font";
  icon = theme "icon";
  cursor = theme "cursor";
  color = theme "color";
}
