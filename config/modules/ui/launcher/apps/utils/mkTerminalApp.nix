{ writeShellScript, coreutils, terminal }:

name: terminalname: bin: writeShellScript name ''
  test=${coreutils}/bin/test

  if $test ! "$WAYLAND_DISPLAY" -o \( "$TERM" -a "$TERM" != "linux" \)
  then
    exec ${bin}
  else
    exec ${terminal} --title ${name} --name ${terminalname} -e "${bin}"
  fi
''
