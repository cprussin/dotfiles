{ writeShellScriptBin, coreutils, gnused, prompt }:

writeShellScriptBin "pinentry" ''
  echo=${coreutils}/bin/echo
  printf=${coreutils}/bin/printf
  tr=${coreutils}/bin/tr
  test=${coreutils}/bin/test
  sed=${gnused}/bin/sed
  prompt=${prompt}

  PROMPT="Enter your passphrase"
  DESC=""

  getPassword() {
    $echo "" | DISPLAY=:0 $prompt \
      -width 25 \
      -p "$PROMPT " \
      -mesg "$DESC" \
      -input /dev/null \
      -password \
      -theme-str "textbox-prompt-colon { enabled: false; }" \
      -lines 0
  }

  $echo OK
  while read cmd rest; do
    case "$cmd" in

      SETDESC)
        DESC="$($echo "$rest" | $sed 's|%0A|\n|g;s|<|\&lt;|g;s|>|\&gt;|g;s|%22|<b>|;s|%22|</b>|')"
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
      BYE) $echo OK; exit ;;
      *) $echo OK ;;

    esac
  done
''
