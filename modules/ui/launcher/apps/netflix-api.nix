{ writeScript, bash }:

writeScript "shakti" ''
  #! ${bash}/bin/sh

  browse=@out@/bin/browse

  case "$1" in
    repl) url='https://api-staging-internal.netflix.com/internal/repl' ;;
    repl-prod) url='https://api-internal.netflix.com/internal/repl' ;;
    repl-int) url='https://api-int-internal.test.netflix.com/internal/repl' ;;
    repl-test) url='https://api-internal.test.netflix.com/internal/repl' ;;
  esac

  exec $browse $url
''
