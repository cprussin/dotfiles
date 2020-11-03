{ callPackage, dircolors-solarized }:
let
  colors = callPackage ./colors.nix { };
  common = callPackage ./common.nix { };
in
common // {
  background = colors.base3;
  foreground = colors.base00;
  highlightBackground = colors.base2;
  highlightForeground = colors.base01;
  secondaryContent = colors.base1;
  dircolors = "${dircolors-solarized}/etc/dircolors/solarized/dircolors.ansi-light";
}
