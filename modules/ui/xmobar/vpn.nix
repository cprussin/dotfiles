{ writeShellScript, systemd, coreutils, config }:

writeShellScript "vpn" ''
  echo=${coreutils}/bin/echo
  systemctl=${systemd}/bin/systemctl

  if $systemctl is-active --quiet openvpn-netflix; then
      $echo "<fc=${config.colorTheme.white},${config.colorTheme.red}> <fn=1>VPN</fn> </fc>    "
  fi
''
