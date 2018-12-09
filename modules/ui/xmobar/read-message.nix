{ writeScript, bash, coreutils, gnused }:

writeScript "read-message" ''
  #! ${bash}/bin/sh

  sed=${gnused}/bin/sed
  test=${coreutils}/bin/test
  paste=${coreutils}/bin/paste

  if $test -f $XDG_RUNTIME_DIR/message
  then
      $sed 's/^[^ ]* \(.*\)/\1  |  /' < $XDG_RUNTIME_DIR/message | $paste -sd ''' -
  fi
''
