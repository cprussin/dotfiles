{ writeShellScript, systemd }:

writeShellScript "presentation" ''
  systemctl=${systemd}/bin/systemctl

  if $systemctl --user is-active --quiet swayidle
  then
    $systemctl --user stop swayidle
  else
    $systemctl --user start swayidle
  fi
''
