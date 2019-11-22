{ pkgs, ... }:

{
  fonts.fonts = [
    pkgs.dejavu_fonts
    pkgs.font-awesome_5
    pkgs.noto-fonts
  ];
}
