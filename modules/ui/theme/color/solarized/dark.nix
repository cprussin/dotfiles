{ callPackage, dircolors-solarized }:

let
  colors = callPackage ./colors.nix {};
  common = callPackage ./common.nix {};
in

common // {
  background = colors.base03;
  foreground = colors.base0;
  highlightBackground = colors.base02;
  highlightForeground = colors.base1;
  secondaryContent = colors.base01;
  dircolors = "${dircolors-solarized}/etc/dircolors/solarized/dircolors.ansi-dark";
}
