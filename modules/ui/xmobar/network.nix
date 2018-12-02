{ writeScript, bash, iproute, gnugrep, gnused, coreutils, wirelesstools }:

writeScript "network" ''
  #! ${bash}/bin/sh

  ip=${iproute}/bin/ip
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  cat=${coreutils}/bin/cat
  cut=${coreutils}/bin/cut
  head=${coreutils}/bin/head
  tail=${coreutils}/bin/tail
  iwgetid=${wirelesstools}/bin/iwgetid

  for file in /sys/class/net/*
  do
      if [ -d $file/wireless ]
      then
          WIRELESS_INTERFACE=''${file#/sys/class/net/}
      elif [ -h $file/device ]
      then
          WIRED_INTERFACE=''${file#/sys/class/net/}
      fi
  done

  function isup() {
      [ "$($cat /sys/class/net/$1/operstate)" == "up" ]
  }

  function getip() {
      $ip addr show dev $1 |\
      $grep "inet " |\
      $cut -f 6 -d ' ' |\
      $sed 's|/.*||' |\
      $head -n 1
  }

  icon="<fn=1></fn>"
  if isup $WIRELESS_INTERFACE; then
      strength=$($tail -n 1 /proc/net/wireless | $cut -d ' ' -f 5 | $sed 's/\.//')
      str="$icon $($iwgetid -r) $(getip $WIRELESS_INTERFACE) [$strength%]"
      if [ $strength -lt 20 ]; then
          echo -n "<fc=#dc322f>$str</fc>    "
      elif [ $strength -lt 50 ]; then
          echo -n "<fc=#859900>$str</fc>    "
      else
          echo -n "$str    "
      fi
  else
      echo -n "<fc=#586e75>$icon</fc>    "
  fi

  if isup $WIRED_INTERFACE; then
      echo -n "<fn=1></fn> $(getip $WIRED_INTERFACE)    "
  fi

  echo
''
