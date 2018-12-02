{ writeScript, bash, coreutils }:

writeScript "ls" ''
  #! ${bash}/bin/sh

  realpath=${coreutils}/bin/realpath
  ls=${coreutils}/bin/ls

  for elem in $@
  do
      if [ -e "$elem" ]
      then
          dir="$elem"
          break
      fi
  done

  if [ "$dir" ]; then
      dir="$($realpath $dir)"
  else
      dir="$(pwd)"
  fi

  echo -e "\033[1mFiles in "$dir"\033[0m:"
  $ls -HF --color=always --group-directories-first $@
  echo
''
