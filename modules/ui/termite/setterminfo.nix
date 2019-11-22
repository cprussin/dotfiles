{ writeShellScriptBin, coreutils, openssh, terminfo }:

writeShellScriptBin "setterminfo" ''
  echo=${coreutils}/bin/echo
  ssh=${openssh}/bin/ssh
  scp=${openssh}/bin/scp

  $echo "Setting terminfo on $1..."
  $scp -r ${terminfo}/share/terminfo $1:~/.terminfo
''
