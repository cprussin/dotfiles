{ writeScript, bash }:

writeScript "volume" ''
  #! ${bash}/bin/sh

  volume=$(volume get)

  if [ $(volume get-mute) == "true" ]; then
      str="<fc=#859900><fn=1></fn> $volume%</fc>"
  else
      if [ $volume -lt 30 ]; then
          symbol=""
      elif [ $volume -lt 70 ]; then
          symbol=""
      else
          symbol=""
      fi
      str="<fn=1>$symbol</fn> $volume%"
  fi

  echo -n "<action=\`$APP_PATH/mixer\` button=13>"
  echo -n "<action=\`volume toggle\` button=2>"
  echo -n "<action=\`volume + 1\` button=4>"
  echo -n "<action=\`volume - 1\` button=5>"
  echo -n $str
  echo -n "</action></action></action></action>"
  echo "    "
''
