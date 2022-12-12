{
  writeShellScript,
  notify-send,
  systemd,
}:
writeShellScript "remacs" ''
  notifySend=${notify-send}/bin/notify-send
  systemctl=${systemd}/bin/systemctl

  if $systemctl --user is-active --quiet emacs
  then
    msgId=$($notifySend -p -t 0 -i emacs "Restarting emacs...")
    $systemctl --user restart emacs
    $notifySend -s $msgId
  fi
''
