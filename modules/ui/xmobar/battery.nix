{ writeScript, bash, acpi, coreutils, gnugrep, findutils }:

writeScript "battery" ''
  #! ${bash}/bin/sh

  acpi=${acpi}/bin/acpi
  wc=${coreutils}/bin/wc
  grep=${gnugrep}/bin/grep
  tr=${coreutils}/bin/tr
  xargs=${findutils}/bin/xargs

  count=$($acpi -b | $wc -l)
  sum=$(
    $acpi -b |\
    $grep -oE '[0-9]{1,3}%' |\
    $tr -d '%' |\
    $xargs -I% echo -n '+%'
  )
  average=$(( sum / count ))

  if [ "$($acpi | grep Full)" ]; then
      status="<fn=1></fn>"
  elif [ "$($acpi | grep Charging)" ]; then
      status="<fn=1> </fn> ▲"
  elif [ $average -lt 50 ]; then
      status="<fn=1> </fn> ▼"
  else
      status="<fn=1> </fn> ▼"
  fi

  str="$status $average%"

  if [ $average -lt 5 ]; then
      str="<fc=#dc322f>$str</fc>"
  elif [ $average -lt 15 ]; then
      str="<fc=#859900>$str</fc>"
  fi

  echo "$str    "
''
