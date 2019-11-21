{ writeShellScript }:

writeShellScript "shakti" ''
  browse=@out@/bin/browse

  case "$1" in
    prod) url='https://api-internal.netflix.com/internal/repl' ;;
    int) url='https://api-int-internal.test.netflix.com/internal/repl' ;;
    test) url='https://api-internal.test.netflix.com/internal/repl' ;;
    *) url='https://api-staging-internal.netflix.com/internal/repl' ;;
  esac

  exec $browse $url
''
