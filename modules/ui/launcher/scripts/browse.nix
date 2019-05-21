{ writeScript, bash, coreutils, chromium, google-chrome, firefox, minichrome, opera }:

writeScript "browse" ''
  #! ${bash}/bin/sh

  chromium=${chromium}/bin/chromium
  chrome=${google-chrome}/bin/google-chrome-stable
  firefox=${firefox}/bin/firefox
  minichrome=${minichrome}/bin/minichrome
  opera=${opera}/bin/opera
  test=${coreutils}/bin/test

  browser="chromium"
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
    chromium) exec $chromium --enable-native-gpu-memory-buffers --app=$target >/dev/null 2>&1 ;;
    chrome) exec $chrome --enable-native-gpu-memory-buffers --app=$target >/dev/null 2>&1 ;;
    firefox) exec $firefox $target >/dev/null 2>&1 ;;
    minichrome) exec $minichrome browse $target >/dev/null 2>&1 ;;
  esac
''
