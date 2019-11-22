{ writeShellScript, notify-send, systemd }:

writeShellScript "syncmail" ''
  notifySend=${notify-send}/bin/notify-send
  systemctl=${systemd}/bin/systemctl

  if ! $systemctl --user is-active --quiet mbsync && ! $systemctl --user is-active --quiet mbsync-manual
  then
    msgId=$($notifySend -p -t 0 -i emblem-synchronizing "Syncing email..." "")
    $systemctl --user start mbsync-manual
    $notifySend -s $msgId
  fi
''
