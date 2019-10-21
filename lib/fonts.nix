{}:

let
  xftFont = fontModule:
    "xft:${fontModule.face}:size=${toString fontModule.size}";

  pangoFont = fontModule:
    "${fontModule.face} ${toString fontModule.size}";
in

{
  inherit xftFont pangoFont;
}
