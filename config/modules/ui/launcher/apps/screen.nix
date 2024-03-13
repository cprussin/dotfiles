{
  writeShellScript,
  coreutils,
  grim,
  slurp,
  sway,
  jq,
  wf-recorder,
}:
writeShellScript "screen" ''
  ls=${coreutils}/bin/ls
  wc=${coreutils}/bin/wc
  date=${coreutils}/bin/date
  test=${coreutils}/bin/test
  grim=${grim}/bin/grim
  wfRecorder=${wf-recorder}/bin/wf-recorder
  slurp=${slurp}/bin/slurp
  swaymsg=${sway}/bin/swaymsg
  jq=${jq}/bin/jq

  if $test "$1" = 'shot'
  then
    prefix="screenshot"
    ext="png"
  else
    prefix="screen-recording"
    ext="mkv"
  fi
  file="$HOME/Scratch/$prefix-$($date +'%Y-%m-%d-%H%M%S').$ext"

  if $test "$2" = 'region'
  then
    geometry="$($slurp)"
    if $test ! "$geometry"
    then
      exit
    fi
  elif $test "$2" = 'output'
  then
    geometry="$($swaymsg -t get_outputs | $jq -r '.[] | select(.active) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | $slurp)"
    if $test ! "$geometry"
    then
      exit
    fi
  elif $test "$2" != 'full'
  then
    geometry="$($swaymsg -t get_tree | $jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | $slurp)"
    if $test ! "$geometry"
    then
      exit
    fi
  fi

  if $test "$1" = 'shot'
  then
    if $test "$geometry"
    then
      exec $grim -g "$geometry" "$file"
    else
      exec $grim "$file"
    fi
  else
    if $test "$geometry"
    then
      exec $wfRecorder -g "$geometry" -f "$file"
    else
      exec $wfRecorder -f "$file"
    fi
  fi
''
