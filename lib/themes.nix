{ pkgs }:

let
  theme = type: name: import (../modules/ui/theme + "/${type}/${name}.nix") {
    inherit pkgs;
  };
in

{
  font = theme "font";
  icon = theme "icon";
  cursor = theme "cursor";
}
