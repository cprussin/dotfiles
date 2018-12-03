{ writeScript, bash, coreutils }:

writeScript "date" ''
  #! ${bash}/bin/sh

  date=${coreutils}/bin/date
  echo=${coreutils}/bin/echo

  currentDate=$($date +"%A %Y-%m-%d %H:%M:%S")
  $echo "<action=\`$APP_PATH/calendar\` button=123>$currentDate</action>"
''
