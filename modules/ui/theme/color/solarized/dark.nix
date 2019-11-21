{ callPackage }:

let
  colors = callPackage ./colors.nix {};
  common = callPackage ./common.nix {};
  dircolors = callPackage ./dircolors.nix {};
in

common // {
  background = colors.base03;
  foreground = colors.base0;
  highlightBackground = colors.base02;
  highlightForeground = colors.base1;
  secondaryContent = colors.base01;
  dircolors = "${dircolors}/dircolors.ansi-dark";
}
