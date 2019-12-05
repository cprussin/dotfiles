{ writeShellScriptBin
, callPackage
, fortune
, findutils
, coreutils
, gnugrep
, gnused
, rofi
}:

let
  open = callPackage ./open.nix {};
  browse = callPackage ./browse.nix {};
  search = callPackage ./search.nix {};
in

writeShellScriptBin "launch" ''
  fortune=${fortune}/bin/fortune
  rofi=${rofi}/bin/rofi
  open=${open}/bin/open
  browse=${browse}/bin/browse
  search=${search}/bin/search
  find=${findutils}/bin/find
  echo=${coreutils}/bin/echo
  sort=${coreutils}/bin/sort
  test=${coreutils}/bin/test
  ls=${coreutils}/bin/ls
  grep=${gnugrep}/bin/grep
  sed=${gnused}/bin/sed

  tlds='com|net|org|gov|edu|co|io|do|me'
  browsePrefixes='http://|go/|localhost|chrome://'
  browseRegex='([0-9]{1,3}\.){3}[0-9]{1,3}'

  showPrompt() {
    $rofi \
      -dmenu -i \
      -show run \
      -columns 4 \
      -lines 7 \
      -p ''' \
      -theme-str "textbox-prompt-colon { enabled: false; }" \
      -mesg "$($fortune)"
  }

  listTopLevelFiles() {
    $find ~/* -maxdepth 1
  }

  listOtherFiles() {
    $find ~/{Documents,Notes,Scratch} -mindepth 2 -not -path '*/\.*'
  }

  listFiles() {
    (listTopLevelFiles; listOtherFiles) | $sed "s|$HOME/||" | $sort
  }

  listApps() {
    $ls "$HOME/.launcher-apps"
  }

  showLauncher() {
    selection=$((listApps; listFiles) | showPrompt)

    if $test "$selection"
    then
      exec $0 $selection
    fi
  }

  runLauncherOnArgs() {
    resolvedPath="$(eval $echo "$@" 2>/dev/null)"

    if $test -x "$HOME/.launcher-apps/$1"
    then
      exec "$HOME/.launcher-apps/$@"
    elif $test "$resolvedPath" -a -e "$resolvedPath"
    then
      exec $open "$resolvedPath"
    elif $test "$resolvedPath" -a -e "$HOME/$resolvedPath"
    then
      exec $open "$HOME/$resolvedPath"
    elif $echo "$1" | $grep -oqE "(\.($TLDS)|^($browsePrefixes)|^($browseRegex)$)"
    then
      exec $browse "$1"
    elif $test "$*"
    then
      exec $search "$*"
    fi
  }

  if $test $# -eq 0
  then
    showLauncher
  else
    runLauncherOnArgs $*
  fi
''
