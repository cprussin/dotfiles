{ writeShellScriptBin, i3lock }:

writeShellScriptBin "lock-screen" ''
  exec ${i3lock}/bin/i3lock -c 000000
''
