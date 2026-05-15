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

  case $browser in
    brave) exec $brave $target >/dev/null 2>&1 ;;
    chromium) exec $chromium $target >/dev/null 2>&1 ;;
    tor-browser) exec $tor $target >/dev/null 2>&1 ;;
    chrome) exec $chrome $target >/dev/null 2>&1 ;;
    firefox) exec $firefox $target >/dev/null 2>&1 ;;
  esac
''
