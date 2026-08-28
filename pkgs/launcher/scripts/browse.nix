{
  writeShellScriptBin,
  coreutils,
  chromium,
  google-chrome,
  firefox,
  brave,
  tor-browser,
}:
writeShellScriptBin "browse" ''
  chromium=${chromium}/bin/chromium
  chrome=${google-chrome}/bin/google-chrome-stable
  firefox=${firefox}/bin/firefox
  brave=${brave}/bin/brave
  tor=${tor-browser}/bin/tor-browser
  test=${coreutils}/bin/test

  browser="brave"
  target=""

  while $test $# -gt 0
  do
    case "$1" in
      --browser) browser="$2"; shift ;;
      *)
        if [[ $1 =~ :\/\/ ]]
        then
            target=$1
        elif [[ $1 =~ ^/ ]]
        then
            target="file://$1"
        else
            target="http://$1"
        fi
        ;;
    esac
    shift
  done

  # Quoted, because this is the registered http/https handler now: a file://
  # URL for a path with a space arrives as one argument and would otherwise
  # split into two.  The :+ keeps an empty target from becoming an empty
  # argument, which is what opens the browser on its homepage.
  case $browser in
    brave) exec $brave ''${target:+"$target"} >/dev/null 2>&1 ;;
    chromium) exec $chromium ''${target:+"$target"} >/dev/null 2>&1 ;;
    tor-browser) exec $tor ''${target:+"$target"} >/dev/null 2>&1 ;;
    chrome) exec $chrome ''${target:+"$target"} >/dev/null 2>&1 ;;
    firefox) exec $firefox ''${target:+"$target"} >/dev/null 2>&1 ;;
  esac
''
