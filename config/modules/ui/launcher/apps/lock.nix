{ writeShellScript, swaylock }:

writeShellScript "lock" ''
  sudo umount /secure
  exec ${swaylock}/bin/swaylock "$@"
''
