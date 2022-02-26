{
  writeShellScript,
  launcher,
}: name: url: writeShellScript name "exec ${launcher}/bin/browse ${url}"
