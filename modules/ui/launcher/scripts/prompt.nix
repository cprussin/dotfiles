{ writeScript, bash, coreutils, rofi }:

writeScript "prompt" ''
  #! ${bash}/bin/sh

  cat=${coreutils}/bin/cat
  rofi=${rofi}/bin/rofi

  $cat - | $rofi \
    -fixed-num-lines \
    -font "DejaVuSansMono 10" \
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
