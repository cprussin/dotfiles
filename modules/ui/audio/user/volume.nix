{ writeScriptBin, bash, pamixer, gnugrep, gnused, coreutils }:

writeScriptBin "volume" ''
  #! ${bash}/bin/sh

  pamixer=${pamixer}/bin/pamixer
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed
  test=${coreutils}/bin/test

  # Get the sink number
  sink=$($pamixer --list-sinks | $grep a2dp | $sed 's/\([0-9]\+\).*/\1/')
  if $test ! "$sink"
  then
      sink=0
  fi

  # First, actually set the volume
  case $1 in
      toggle) $pamixer --sink $sink --toggle-mute ;;
      +) $pamixer --sink $sink --increase $2 ;;
      -) $pamixer --sink $sink --decrease $2 ;;
      get-mute)
          $pamixer --sink $sink --get-mute
          exit
          ;;
      get)
          $pamixer --sink $sink --get-volume
          exit
          ;;
  esac

  # FIXME get aosd installed and re-enable this
  ## Next, set the contents of the on screen message
  #[ $(pamixer --sink $sink --get-mute) == 'true' ] && muted=true
  #[ $muted ] && mute='' || mute=''
  #message="$mute:$(pamixer --sink $sink --get-volume)%"
  #[ $muted ] && color='#99ff00' || color='#0099ff'
  #
  ## Now, show the on-screen display, ensuring that the old one is removed
  ## immediately if it's still running
  #pid=$(ps aux | grep aosd_cat | head -n 1 | cut -d ' ' -f 2)
  #echo $message | aosd_cat --font="DejaVu Sans 55, Icons 55" \
  #                         --fore-color=$color --back-color=black \
  #                         --back-opacity 255 --shadow-opacity=0 \
  #                         --fade-in=0 --fade-full=500 --fade-out=0 \
  #                         --padding=30 &
  #kill -9 $pid
''
