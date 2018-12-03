{ writeScript, bash, coreutils }:

writeScript "email" ''
  #! ${bash}/bin/sh

  ls=${coreutils}/bin/ls
  wc=${coreutils}/bin/wc
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo

  str=""
  for mailbox in $($ls -1 $HOME/Mail); do
      cur=$($ls -1 $HOME/Mail/$mailbox/Inbox/cur | $wc -l)
      new=$($ls -1 $HOME/Mail/$mailbox/Inbox/new | $wc -l)
      total=$((cur + new))

      case $mailbox in
          "GMail") action="$APP_PATH/gmail" ;;
          "Netflix") action="$APP_PATH/gmail netflix" ;;
          *) action="$APP_PATH/email" ;;
      esac

      if $test $total -gt 0
      then
          mailbox="<action=\`$action\` button=123> $mailbox <fn=1></fn> $total</action>"
          $test $new -ne 0 && mailbox="<fc=#859900>$mailbox</fc>"
          str="$str$mailbox    "
      fi
  done

  $echo "$str"
''
