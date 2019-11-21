{ writeShellScript, coreutils, xdotool, imagemagick }:

writeShellScript "screenshot" ''
  ls=${coreutils}/bin/ls
  wc=${coreutils}/bin/wc
  test=${coreutils}/bin/test
  xdotool=${xdotool}/bin/xdotool
  import=${imagemagick}/bin/import

  num=$($ls -1 $HOME/Scratch/screenshot*.png 2>/dev/null | $wc -l)

  if $test $1 == 'region'
  then
    args=""
  else
    args="-window $($xdotool selectwindow)"
  fi

  exec $import $args $HOME/Scratch/screenshot$num.png
''
