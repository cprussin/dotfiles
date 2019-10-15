{ writeScript, bash, coreutils, rofi, config }:

writeScript "prompt" ''
  #! ${bash}/bin/sh

  cat=${coreutils}/bin/cat
  rofi=${rofi}/bin/rofi

  $cat - | $rofi \
    -fixed-num-lines \
    -font "${config.primaryFont.face} ${toString config.primaryFont.size}" \
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
