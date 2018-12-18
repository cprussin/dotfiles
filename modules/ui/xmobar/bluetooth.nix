{ writeScript, bash, bluez, gnugrep, gnused, coreutils, launcher }:

writeScript "bluetooth" ''
  #! ${bash}/bin/sh

  bluetoothctl=${bluez}/bin/bluetoothctl
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  paste=${coreutils}/bin/paste
  echo=${coreutils}/bin/echo
  test=${coreutils}/bin/test
  bluetooth=${launcher}/share/apps/bluetooth

  function isup() {
    $test "$($bluetoothctl show 2>/dev/null | $grep Powered | $grep yes)"
  }

  function devices() {
    $bluetoothctl devices 2>/dev/null |\
    $grep Device |\
    $sed 's/Device \([^ ]*\) .*/info \1/' |\
    $bluetoothctl 2>/dev/null |\
    $grep -B 7 'Connected: yes' |\
    $grep Alias |\
    $sed 's/.*Alias: //' |\
    $paste -s - |\
    $sed 's/\t/, /'
  }

  str="<fn=3></fn>"
  if isup
  then
      togglePower="off"
      devs=$(devices)
      if $test "$devs"
      then
          str="$str $devs"
      else
          str="<fc=#859900>$str</fc>"
      fi
  else
      togglePower="on"
      str="<fc=#586e75>$str</fc>"
  fi
  str="<action=\`echo \"power $togglePower\" | $bluetoothctl\` button=3>$str</action>"
  str="<action=\`$bluetooth\` button=12>$str</action>"

  $echo "$str    "
''
