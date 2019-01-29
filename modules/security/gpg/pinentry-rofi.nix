{ writeScript, bash, rofi, coreutils, gnused }:

writeScript "pinentry-rofi" ''
  #! ${bash}/bin/sh

  echo=${coreutils}/bin/echo
  printf=${coreutils}/bin/printf
  tr=${coreutils}/bin/tr
  test=${coreutils}/bin/test
  rofi=${rofi}/bin/rofi
  sed=${gnused}/bin/sed

  PROMPT="Enter your passphrase"
  DESC=""

  getPassword() {
    DISPLAY=:0 $rofi \
      -font "DejaVuSansMono 10" \
      -color-normal "#002b36,#657b83,#002b36,#859900,#002b36" \
      -color-window "#002b36,#859900" \
      -bw 2 \
      -width 20 \
      -dmenu \
      -p "$PROMPT " \
      -mesg "$($echo "$DESC" | $sed 's|%0A|\n|g;s|<|\&lt;|g;s|>|\&gt;|g;s|%22|<b>|;s|%22|</b>|')" \
      -input /dev/null \
      -password \
      -theme-str "textbox-prompt-colon { enabled: false; }" \
      -lines 0
  }

  $echo OK
  while read cmd rest; do
    case "$cmd" in

      SETDESC)
        DESC=$rest
        if $test ''${DESC: -3} != '%0A'
        then
            DESC="$DESC%0A"
        fi
        $echo OK
        ;;

      GETINFO)
        case $rest in
          pid*) $echo -e "D $$\nOK" ;;
          version) $echo -e "D 1.0.0\nOK" ;;
          flavor*) $echo -e "D curses:curses\nOK" ;;
          ttyinfo*) $echo -e "D - - -\nOK" ;;
        esac
      ;;

      GETPIN) $echo -e "D $(getPassword)\nOK" ;;
      CONFIRM) $echo ASSUAN_Not_Confirmed ;;
      SETPROMPT) PROMPT=$rest; $echo OK ;;
      SETOK) OK=$rest; $echo OK ;;
      SETERROR) ERROR=$rest; $echo OK ;;
      OPTION) $echo OK ;;
      BYE) $echo OK; exit ;;
      *) $echo OK ;;

    esac
  done
''
