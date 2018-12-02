{ writeScript, bash, coreutils }:

writeScript "message" ''
  #! ${bash}/bin/sh

  if [ -f $XDG_RUNTIME_DIR/message ]; then
      IFS=$'\n'
      for line in $(${coreutils}/bin/cat $XDG_RUNTIME_DIR/message)
      do
          echo -n "$line  |  "
      done
  fi
  echo
''
