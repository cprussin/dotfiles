{ writeScript, bash, dunst, systemd }:

writeScript "syncmail" ''
  #! ${bash}/bin/sh

  dunstify=${dunst}/bin/dunstify
  systemctl=${systemd}/bin/systemctl

  if ! $systemctl --user is-active --quiet mbsync -a ! $systemctl --user is-active --quiet mbsync-manual
  then
    $dunstify -t 0 -r 47646 -i emblem-synchronizing "Syncing email..."
    $systemctl --user start mbsync-manual
    $dunstify -C 47646
  fi
''
