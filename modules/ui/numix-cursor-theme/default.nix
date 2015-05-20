{ pkgs, ... }:

{
  xsession.pointerCursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix";
  };
}
