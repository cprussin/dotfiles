{ writeScriptBin, bash, coreutils, openssh }:

writeScriptBin "setterminfo" ''
  #! ${bash}/bin/sh

  echo=${coreutils}/bin/echo
  ssh=${openssh}/bin/ssh
  scp=${openssh}/bin/scp

  $echo "Setting terminfo on $1..."
  $ssh $1 mkdir -p ~/.terminfo/r
  $scp /usr/share/terminfo/r/rxvt-unicode-256color $1:.terminfo/r
''
