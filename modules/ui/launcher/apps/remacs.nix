{ writeScript, bash, dunst, systemd }:

writeScript "remacs" ''
  #! ${bash}/bin/sh

  dunstify=${dunst}/bin/dunstify
  systemctl=${systemd}/bin/systemctl

  if $systemctl --user is-active --quiet emacs-daemon
  then
    $dunstify -t 0 -r 47645 "" "Restarting emacs..."
    $systemctl --user restart emacs-daemon
    $dunstify -C 47645
  fi
''
