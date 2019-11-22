{ callPackage }:

let
  stripOverrides = callPackage ./stripOverrides.nix {};

  theme = type: name:
    stripOverrides (
      callPackage (../modules/ui/theme + "/${type}/${name}.nix") {}
    );
in

{
  color = theme "color";
  cursor = theme "cursor";
  font = theme "font";
  icon = theme "icon";
  keymap = theme "keymap";
}
