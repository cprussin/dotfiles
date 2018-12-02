{ writeScript, bash, coreutils }:

writeScript "date" ''
  #! ${bash}/bin/sh

  date=$(${coreutils}/bin/date +"%A %Y-%m-%d %H:%M:%S")
  echo -n "<action=\`$APP_PATH/calendar\` button=123>$date</action>"
''
