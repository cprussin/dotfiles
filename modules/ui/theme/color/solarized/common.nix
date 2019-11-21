{ callPackage }:

let
  colors = callPackage ./colors.nix {};
in

{
  black = colors.base02;
  white = colors.base3;
  grey = colors.base00;
  lightGrey = colors.base2;
  red = colors.red;
  lightRed = colors.orange;
  green = colors.green;
  lightGreen = colors.base01;
  yellow = colors.yellow;
  lightYellow = colors.base00;
  blue = colors.blue;
  lightBlue = colors.base0;
  purple = colors.magenta;
  lightPurple = colors.violet;
  cyan = colors.cyan;
  lightCyan = colors.base1;

  selection = colors.green;
  bright = colors.blue;
  urgent = colors.red;
  warn = colors.green;
}
