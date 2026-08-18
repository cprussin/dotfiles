{
  writeShellScript,
  launcher,
}: name: url:
writeShellScript name ''
  case $1 in
    pyth) USER=connor@dourolabs.xyz ;;
    zeit) USER=connor@ac-zeitgeist.com ;;
    zeitgeist) USER=connor@ac-zeitgeist.com ;;
    *) USER=connor@prussin.net ;;
  esac
  exec ${launcher}/bin/browse ${url}?authuser=$USER
''
