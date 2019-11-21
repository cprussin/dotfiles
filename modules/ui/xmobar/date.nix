{ writeShellScript, coreutils, launcher }:

writeShellScript "date" ''
  date=${coreutils}/bin/date
  echo=${coreutils}/bin/echo
  calendar=${launcher}/share/apps/calendar

  currentDate=$($date +"%A %Y-%m-%d %H:%M:%S")
  $echo "<action=\`$calendar\` button=123>$currentDate</action>"
''
