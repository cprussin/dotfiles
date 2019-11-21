{ writeShellScript, inotify-tools, systemd }:

writeShellScript "watch-monitor-change" ''
  inotifywait=${inotify-tools}/bin/inotifywait
  systemctl=${systemd}/bin/systemctl

  while $inotifywait /run/display-change
  do
    $systemctl --user start setup-monitors
  done
''
