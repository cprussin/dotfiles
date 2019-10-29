{ writeScript, bash, coreutils, volume, launcher, config }:

writeScript "volume" ''
  #! ${bash}/bin/sh

  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  volume=${volume}/bin/volume
  mixer=${launcher}/share/apps/mixer

  currentVolume=$($volume get)

  if $test $($volume get-mute) == "true"
  then
      str="<fc=${config.colorTheme.foreground}><fn=2></fn> $currentVolume</fc>"
  else
      if $test $currentVolume -lt 30
      then
          symbol=""
      elif $test $currentVolume -lt 70
      then
          symbol=""
      else
          symbol=""
      fi
      str="<fn=2>$symbol</fn> $currentVolume"
  fi

  str="<action=\`$volume - 1\` button=5>$str</action>"
  str="<action=\`$volume + 1\` button=4>$str</action>"
  str="<action=\`$volume toggle\` button=2>$str</action>"
  str="<action=\`$mixer\` button=13>$str</action>"

  $echo "$str    "
''
