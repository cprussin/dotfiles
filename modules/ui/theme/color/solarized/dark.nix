{ pkgs }:

let
  colors = import ./colors.nix;
  common = import ./common.nix;
  dircolors = pkgs.callPackage ./dircolors.nix {};
in

common // {
  background = colors.base03;
  foreground = colors.base0;
  highlightBackground = colors.base02;
  highlightForeground = colors.base1;
  secondaryContent = colors.base01;
  dircolors = "${dircolors}/dircolors.ansi-dark";
}
