{ writeScript, bash, utillinux, gnugrep, coreutils }:

writeScript "mounts" ''
  #! ${bash}/bin/sh

  mount=${utillinux}/bin/mount
  grep=${gnugrep}/bin/grep
  echo=${coreutils}/bin/echo

  checkMount() {
    $mount | $grep "$1" >/dev/null
  }

  if checkMount "/secure"
  then
    $echo -n "<fn=2></fn>    "
  fi

  if checkMount "/boot"
  then
    $echo -n "<fn=2></fn>    "
  fi
''
