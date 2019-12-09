{ writeShellScriptBin, coreutils, chromium, google-chrome, firefox, brave }:

writeShellScriptBin "browse" ''
  chromium=${chromium}/bin/chromium
  chrome=${google-chrome}/bin/google-chrome-stable
  firefox=${firefox}/bin/firefox
  brave=${brave}/bin/brave
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
        elif [[ $1 =~ ^go/ ]]
        then
            target="https://go.netflix.com/''${1##go/}"
        else
            target="http://$1"
        fi
        ;;
    esac
    shift
  done

  case $browser in
    brave) exec $brave --enable-native-gpu-memory-buffers --app=$target >/dev/null 2>&1 ;;
    chromium) exec $chromium --enable-native-gpu-memory-buffers --app=$target >/dev/null 2>&1 ;;
    chrome) exec $chrome --enable-native-gpu-memory-buffers --app=$target >/dev/null 2>&1 ;;
    firefox) exec $firefox $target >/dev/null 2>&1 ;;
  esac
''
