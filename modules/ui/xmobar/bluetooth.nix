{ writeScript, bash, bluez, gnugrep, gnused, coreutils }:

writeScript "bluetooth" ''
  #! ${bash}/bin/sh

  bluetoothctl=${bluez}/bin/bluetoothctl
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  paste=${coreutils}/bin/paste

  function isup() {
    [ "$(echo show | $bluetoothctl 2>/dev/null | $grep Powered | $grep yes)" ]
  }

  function devices() {
    echo devices |\
      $bluetoothctl 2>/dev/null |\
      $grep Device |\
      $sed 's/Device \([^ ]*\) .*/info \1/' |\
      $bluetoothctl 2>/dev/null |\
      $grep -B 8 'Connected: yes' |\
      $grep Name |\
      $sed 's/.*Name: //' |\
      $paste -s - |\
      $sed 's/\t/, /'
  }

  str="<fn=3></fn>"
  if isup; then
      togglePower="off"
      devs=$(devices)
      if [ "$devs" ]; then
          str="$str $devs"
      else
          str="<fc=#859900>$str</fc>"
      fi
  else
      togglePower="on"
      str="<fc=#586e75>$str</fc>"
  fi
  echo -n "<action=\`$APP_PATH/bluetooth\` button=12>"
  echo -n "<action=\`echo \"power $togglePower\" | $bluetoothctl\` button=3>"
  echo -n $str
  echo -n "</action></action>    "

  echo
''
