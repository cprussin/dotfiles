{ writeScript, bash, systemd, coreutils, config }:

writeScript "vpn" ''
  #! ${bash}/bin/sh

  echo=${coreutils}/bin/echo
  systemctl=${systemd}/bin/systemctl

  if $systemctl is-active --quiet openvpn-netflix; then
      $echo "<fc=${config.colorTheme.white},${config.colorTheme.red}> <fn=1>VPN</fn> </fc>    "
  fi
''
