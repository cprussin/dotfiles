{
  writeShellScript,
  launcher,
}: name: url:
writeShellScript name ''
  case $1 in
    pyth) USER=connor@dourolabs.xyz ;;
    getsharpe) USER=cprussin@getsharpe.io ;;
    *) USER=connor@prussin.net ;;
  esac
  exec ${launcher}/bin/browse ${url}?authuser=$USER
''
