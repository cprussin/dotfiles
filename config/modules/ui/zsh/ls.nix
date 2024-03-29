{
  writeShellScript,
  coreutils,
  eza,
}:
writeShellScript "ls" ''
  echo=${coreutils}/bin/echo
  realpath=${coreutils}/bin/realpath
  eza=${eza}/bin/eza
  pwd=${coreutils}/bin/pwd
  test=${coreutils}/bin/test

  printHeader() {
    $echo -e "\033[1mFiles in $1\033[0m:"
  }

  lsWithArgs() {
    $eza --icons -HF --color=always --group-directories-first "$@"
  }

  if $test $# -eq 0
  then
    printHeader "$($pwd)"
    lsWithArgs
  elif $test $# -eq 1 -a -d "$1"
  then
    printHeader "$($realpath "$1")"
    lsWithArgs "$1"
  else
    lsWithArgs "$@"
  fi

  $echo
''
