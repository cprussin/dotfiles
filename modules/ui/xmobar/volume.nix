{ writeScript, bash, coreutils }:

writeScript "volume" ''
  #! ${bash}/bin/sh

  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo

  volume=$(volume get)

  if $test $(volume get-mute) == "true"
  then
      str="<fc=#859900><fn=1></fn> $volume%</fc>"
  else
      if $test $volume -lt 30
      then
          symbol=""
      elif $test $volume -lt 70
      then
          symbol=""
      else
          symbol=""
      fi
      str="<fn=1>$symbol</fn> $volume%"
  fi

  str="<action=\`volume - 1\` button=5>$str</action>"
  str="<action=\`volume + 1\` button=4>$str</action>"
  str="<action=\`volume toggle\` button=2>$str</action>"
  str="<action=\`$APP_PATH/mixer\` button=13>$str</action>"

  $echo "$str    "
''
