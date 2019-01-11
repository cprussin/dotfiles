{ writeScript, bash, inotify-tools, systemd }:

writeScript "watch-monitor-change" ''
  #! ${bash}/bin/sh

  inotifywait=${inotify-tools}/bin/inotifywait
  systemctl=${systemd}/bin/systemctl

  while $inotifywait /run/display-change
  do
    systemctl --user start setup-monitors
  done
''
