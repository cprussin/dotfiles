{callPackage}: let
  colors = callPackage ./colors.nix {};
in {
  inherit (colors) red green yellow blue cyan;

  black = colors.base02;
  white = colors.base3;
  grey = colors.base00;
  lightGrey = colors.base2;
  lightRed = colors.orange;
  lightGreen = colors.base01;
  lightYellow = colors.base00;
  lightBlue = colors.base0;
  purple = colors.magenta;
  lightPurple = colors.violet;
  lightCyan = colors.base1;

  selection = colors.green;
  bright = colors.blue;
  urgent = colors.red;
  warn = colors.yellow;
}
