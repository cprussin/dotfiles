{ writeShellScript, notify-send, systemd }:

writeShellScript "remacs" ''
  notifySend=${notify-send}/bin/notify-send
  systemctl=${systemd}/bin/systemctl

  if $systemctl --user is-active --quiet emacs-daemon
  then
    msgId=$($notifySend -p -t 0 "Restarting emacs...")
    $systemctl --user restart emacs-daemon
    $notifySend -s $msgId
  fi
''
