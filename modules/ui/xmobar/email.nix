{ writeScript, bash, coreutils, launcher }:

writeScript "email" ''
  #! ${bash}/bin/sh

  ls=${coreutils}/bin/ls
  wc=${coreutils}/bin/wc
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  gmail=${launcher}/share/apps/gmail
  email=${launcher}/share/apps/email

  str=""
  for mailbox in $($ls -1 $HOME/Mail); do
      cur=$($ls -1 $HOME/Mail/$mailbox/Inbox/cur | $wc -l)
      new=$($ls -1 $HOME/Mail/$mailbox/Inbox/new | $wc -l)
      total=$((cur + new))

      case $mailbox in
          "GMail") action="$gmail" ;;
          "Netflix") action="$gmail netflix" ;;
          *) action="$email" ;;
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
