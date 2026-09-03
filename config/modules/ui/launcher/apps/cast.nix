{
  writeShellScript,
  systemd,
}:
writeShellScript "cast" ''
  systemctl=${systemd}/bin/systemctl

  # Match on the state rather than `is-active --quiet`'s exit status: that
  # reports failure while the service is still `activating`, so a second `cast`
  # during startup would issue another `start` instead of stopping.
  case "$($systemctl --user is-active sunshine)" in
    inactive | failed)
      $systemctl --user start sunshine
      ;;
    *)
      $systemctl --user stop sunshine
      ;;
  esac
''
