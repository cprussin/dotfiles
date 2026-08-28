{
  writeShellScript,
  brave,
}: name: url:
writeShellScript name ''
  exec ${brave}/bin/brave \
    --app=${url} \
    --user-data-dir="$HOME/.config/${name}" \
    >/dev/null 2>&1
''
