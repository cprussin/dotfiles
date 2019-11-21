{ writeShellScript, callPackage, coreutils, rofi, config }:

let
  fontLib = callPackage ../../../../lib/fonts.nix {};
  colors = config.colorTheme;
in

writeShellScript "prompt" ''
  cat=${coreutils}/bin/cat
  rofi=${rofi}/bin/rofi

  $cat - | $rofi \
    -fixed-num-lines \
    -font "${fontLib.pangoFont config.fontTheme.primaryFont}" \
    -color-normal "${colors.background},${colors.foreground},${colors.background},${colors.selection},${colors.background}" \
    -color-window "${colors.background},${colors.selection}" \
    -bw 2 \
    -location 0 \
    -disable-history \
    -i \
    -modi run \
    -dmenu \
    "$@" \
    -show run
''
