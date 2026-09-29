{
  writeShellScript,
  notify-send,
  emacs,
}:
writeShellScript "remacs" ''
  notifySend=${notify-send}/bin/notify-send
  emacsclient=${emacs}/bin/emacsclient

  # This display's daemon, if it is running; `-a false` keeps the check from
  # starting one.
  if $emacsclient -a false -e t >/dev/null 2>&1
  then
    msgId=$($notifySend -p -t 0 -i emacs "Restarting emacs...")
    $emacsclient -a false -e '(kill-emacs)'
    $emacsclient -e t >/dev/null
    $notifySend -s $msgId
  fi
''
