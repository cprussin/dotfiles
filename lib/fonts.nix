{ lib }:

let
  xftFont = fontModule:
    "xft:${fontModule.face}:size=${toString fontModule.size}";
in

{
  inherit xftFont;
}
