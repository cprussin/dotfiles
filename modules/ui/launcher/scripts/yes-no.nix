{ writeScript, bash, coreutils }:

writeScript "yes-no" ''
  #! ${bash}/bin/sh

  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  prompt=@out@/bin/prompt

  yes="Yes"
  no="No"
  message="Are you sure?"
  cmd="echo yes"

  while $test $# -gt 0
  do
    case "$1" in
      -y|--yes) yes="$2"; shift ;;
      -n|--no) no="$2"; shift ;;
      -m|--message) message="$2"; shift ;;
      --)
        shift
        if $test "$1"
        then
          cmd=$@
        fi
        break
        ;;
    esac
    shift
  done

  selection=$($echo -e "$no\n$yes" | $prompt -lines 1 -p "$message ")
  if $test "$selection" == "$yes"
  then
    exec $cmd
  fi
''