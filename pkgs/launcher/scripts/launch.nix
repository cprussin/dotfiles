{ writeShellScriptBin
, callPackage
, findutils
, coreutils
, gnused
, fortune
, fzf
}:

let
  run = callPackage ./run.nix {};
  preview = callPackage ./preview.nix {};
in

writeShellScriptBin "launch" ''
  fzf=${fzf}/bin/fzf
  find=${findutils}/bin/find
  fortune=${fortune}/bin/fortune
  echo=${coreutils}/bin/echo
  sort=${coreutils}/bin/sort
  test=${coreutils}/bin/test
  ls=${coreutils}/bin/ls
  sed=${gnused}/bin/sed
  run=${run}/bin/run
  preview=${preview}/bin/preview

  showPrompt() {
    $fzf \
      --preview "$preview {}" \
      --exact \
      --print-query \
      --layout=reverse \
      --header "$($fortune)"
  }

  listTopLevelFiles() {
    $find ~/* -maxdepth 1
  }

  listOtherFiles() {
    $find ~/{Notes,Scratch} -mindepth 2 -not -path '*/\.*'
  }

  files() {
    (listTopLevelFiles; listOtherFiles) | $sed "s|$HOME/||" | $sort
  }

  apps() {
    $ls "$HOME/.launcher-apps"
  }

  input="$((apps; files) | showPrompt)"
  selection="$($echo "$input" | $sed -n '2p')"
  query="$($echo "$input" | $sed -n '1p')"

  if $test "$selection"
  then
    exec $* $run "$selection"
  elif $test "$query"
  then
    exec $* $run "$query"
  else
    echo "No selection!"
    exit 1
  fi
''
