{
  writeShellScript,
  coreutils,
  config,
}: name: terminalname: bin:
writeShellScript name ''
  test=${coreutils}/bin/test

  if $test "$WAYLAND_DISPLAY"
  then
    exec ${config.primary-user.home-manager.default-terminal.bin} \
      --title ${name} \
      --class ${terminalname} \
      --name ${terminalname} \
      ${bin}
  else
    exec ${bin}
  fi
''
