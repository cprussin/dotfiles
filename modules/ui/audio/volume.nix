{ writeShellScriptBin, pamixer, gnugrep, gnused, coreutils, dunst, mk-progress-string }:

writeShellScriptBin "volume" ''
  pamixer=${pamixer}/bin/pamixer
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  test=${coreutils}/bin/test
  head=${coreutils}/bin/head
  tail=${coreutils}/bin/tail
  cut=${coreutils}/bin/cut
  dunstify=${dunst}/bin/dunstify
  mkProgressString=${mk-progress-string}/bin/mkProgressString

  # Get the sink number
  sink=$($pamixer --list-sinks | $grep a2dp | $sed 's/\([0-9]\+\).*/\1/')
  if $test ! "$sink"
  then
      sink=$($pamixer --list-sinks | $head -n 2 | $tail -n 1 | $cut -d ' ' -f 1)
  fi

  # First, actually set the volume
  case $1 in
      toggle) $pamixer --sink $sink --toggle-mute ;;
      +) $pamixer --sink $sink --increase $2 ;;
      -) $pamixer --sink $sink --decrease $2 ;;
      get-mute)
          $pamixer --sink $sink --get-mute
          exit
          ;;
      get)
          $pamixer --sink $sink --get-volume
          exit
          ;;
  esac

  # Next, set the contents of the on screen message
  level=$($0 get)
  if $test $($0 get-mute) == 'true'
  then
      icon=notification-audio-volume-muted
      color="#073642"
  elif $test $level -ge 50
  then
      icon=notification-audio-volume-high
  elif $test $level -ge 25
  then
      icon=notification-audio-volume-medium
  elif $test $level -eq 0
  then
      icon=notification-audio-volume-muted
  else
      icon=notification-audio-volume-low
  fi

  $dunstify -i $icon -t 4000 -r 5454 -u normal "" "<span color='$color'>$($mkProgressString $level)</span>"
''
