{ writeShellScript, coreutils, grim, slurp, sway, jq }:

writeShellScript "screenshot" ''
  ls=${coreutils}/bin/ls
  wc=${coreutils}/bin/wc
  date=${coreutils}/bin/date
  test=${coreutils}/bin/test
  grim=${grim}/bin/grim
  slurp=${slurp}/bin/slurp
  swaymsg=${sway}/bin/swaymsg
  jq=${jq}/bin/jq

  file="$HOME/Scratch/screenshot-$($date +'%Y-%m-%d-%H%M%S').png"

  if $test "$1" = 'region'
  then
    exec $grim -g "$($slurp)" "$file"
  elif $test "$1" = 'output'
  then
    exec $grim -g "$($swaymsg -t get_outputs | $jq -r '.[] | select(.active) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | $slurp)" "$file"
  elif $test "$1" = 'full'
  then
    exec $grim "$file"
  else
    exec $grim -g "$($swaymsg -t get_tree | $jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | $slurp)" "$file"
  fi
''
