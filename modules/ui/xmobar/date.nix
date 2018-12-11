{ writeScript, bash, coreutils, launcher }:

writeScript "date" ''
  #! ${bash}/bin/sh

  date=${coreutils}/bin/date
  echo=${coreutils}/bin/echo
  calendar=${launcher}/share/apps/calendar

  currentDate=$($date +"%A %Y-%m-%d %H:%M:%S")
  $echo "<action=\`$calendar\` button=123>$currentDate</action>"
''
