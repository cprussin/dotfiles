{ pkgs }:

let
  colors = import ./colors.nix;
  common = import ./common.nix;
  dircolors = pkgs.callPackage ./dircolors.nix {};
in

common // {
  background = colors.base3;
  foreground = colors.base00;
  highlightBackground = colors.base2;
  highlightForeground = colors.base01;
  secondaryContent = colors.base1;
  dircolors = "${dircolors}/dircolors.ansi-light";
}
