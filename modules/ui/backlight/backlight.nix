{ writeShellScriptBin, coreutils, xorg, dunst, mk-progress-string }:

writeShellScriptBin "backlight" ''
  mkProgressString=${mk-progress-string}/bin/mkProgressString
  xbacklight=${xorg.xbacklight}/bin/xbacklight
  dunstify=${dunst}/bin/dunstify
  printf=${coreutils}/bin/printf
  test=${coreutils}/bin/test

  case $1 in
    +) $xbacklight + $2 ;;
    -) $xbacklight - $2 ;;
  esac

  level=$($printf "%.0f\n" $($xbacklight -get))

  if $test $level -eq 100
  then
    icon=notification-display-brightness-full
  elif $test $level -ge 50
  then
    icon=notification-display-brightness-high
  elif $test $level -ge 25
  then
    icon=notification-display-brightness-medium
  elif $test $level -ge 5
  then
    icon=notification-display-brightness-low
  else
    icon=notification-display-brightness-off
  fi

  $dunstify -i $icon -t 4000 -r 66347 -u normal "" "$($mkProgressString $level)"
''
