{ writeScript, bash, fortune, findutils, coreutils, gnugrep, gnused }:

writeScript "launch" ''
  #! ${bash}/bin/sh

  fortune=${fortune}/bin/fortune
  prompt=@out@/bin/prompt
  open=@out@/bin/open
  browse=@out@/bin/browse
  search=@out@/bin/search
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
      $prompt \
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
      $ls "@out@/share/apps"
  }

  if $test $# -eq 0
  then
      selection=$((listApps; listFiles) | showPrompt)

      if $test "$selection"
      then
          exec $0 $selection
      else
          exit
      fi
  fi

  resolvedPath="$(eval $echo "$@" 2>/dev/null)"

  if $test -x @out@/share/apps/$1
  then
      exec "@out@/share/apps/$@"
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
''
