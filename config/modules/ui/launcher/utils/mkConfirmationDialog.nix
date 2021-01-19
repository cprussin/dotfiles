{ callPackage, coreutils, fzf, config }:
let
  mkModal = callPackage ./mkModal.nix { inherit config; };
in
name: yes: no: prompt: cmd: mkModal name ''
  test=${coreutils}/bin/test
  echo=${coreutils}/bin/echo
  fzf=${fzf}/bin/fzf

  prompt() {
    $fzf --layout=reverse --prompt "${prompt} "
  }

  selection=$($echo -e "${no}\n${yes}" | prompt)
  if $test "$selection" == "${yes}"
  then
    exec ${cmd}
  fi
''
