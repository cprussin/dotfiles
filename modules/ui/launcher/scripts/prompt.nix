{ writeScript, callPackage, bash, coreutils, rofi, config }:

let
  fontLib = callPackage ../../../../lib/fonts.nix {};
in

writeScript "prompt" ''
  #! ${bash}/bin/sh

  cat=${coreutils}/bin/cat
  rofi=${rofi}/bin/rofi

  $cat - | $rofi \
    -fixed-num-lines \
    -font "${fontLib.pangoFont config.fontTheme.primaryFont}" \
    -color-normal "#002b36,#657b83,#002b36,#859900,#002b36" \
    -color-window "#002b36,#859900" \
    -bw 2 \
    -location 0 \
    -disable-history \
    -i \
    -modi run \
    -dmenu \
    "$@" \
    -show run
''
