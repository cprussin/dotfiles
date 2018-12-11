{ writeScript, bash, coreutils, xorg, gnugrep, gnused }:

writeScript "gaming" ''
  #! ${bash}/bin/sh

  xinput=${xorg.xinput}/bin/xinput
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  head=${coreutils}/bin/head
  echo=${coreutils}/bin/echo
  us=@out@/share/apps/us
  dvorak=@out@/share/apps/dvorak

  INPUT=$($xinput list | $grep TouchPad | $sed 's/.*id=\([0-9]*\).*/\1/' | $head -n 1)
  PROP=$($xinput list-props $INPUT | $grep "Disable While Typing" | $grep -v 'Default' | $head -n 1)
  PROPNUM=$($echo $PROP | $sed 's/.*(\(.*\)).*/\1/')

  game_on() {
    $xinput set-int-prop $INPUT $PROPNUM 8 0
    $us
  }

  game_off() {
    $xinput set-int-prop $INPUT $PROPNUM 8 1
    $dvorak
  }

  case "$1" in
    off) game_off ;;
    *) game_on ;;
  esac
''
