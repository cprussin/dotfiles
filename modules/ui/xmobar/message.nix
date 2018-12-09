{ writeScriptBin, bash, coreutils, gnused }:

writeScriptBin "message" ''
  #! ${bash}/bin/sh

  tail=${coreutils}/bin/tail
  cut=${coreutils}/bin/cut
  echo=${coreutils}/bin/echo
  sed=${gnused}/bin/sed
  touch=${coreutils}/bin/touch
  test=${coreutils}/bin/test

  MESSAGE_FILE="$XDG_RUNTIME_DIR/message"

  nextMessageId() {
      previousId=$($tail -n 1 < $MESSAGE_FILE | $cut -d ' ' -f 1)
      $echo $((previousId + 1))
  }

  writeMessage() {
      message="$1"
      program="$2"
      messageId=$(nextMessageId)

      $echo "$messageId $message" >> $MESSAGE_FILE

      if $test "$program"
      then
          trap "clearMessage $messageId" EXIT
          $program
      else
          $echo $messageId
      fi
  }

  clearMessage() {
      messageId="$1"
      $sed -i "/^$messageId /d" $MESSAGE_FILE
  }

  if $test ! -f $MESSAGE_FILE
  then
      $touch $MESSAGE_FILE
  fi

  case $1 in
      write) writeMessage "$2" "$3" ;;
      clear) clearMessage "$2" ;;
      *) $echo "Usage: $0 [clear <id>|write <message> (program)]";;
  esac
''
