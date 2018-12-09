{ writeScript, bash, systemd, coreutils }:

writeScript "vpn" ''
  #! ${bash}/bin/sh

  echo=${coreutils}/bin/echo
  systemctl=${systemd}/bin/systemctl

  if $systemctl is-active --quiet openvpn-netflix; then
      $echo "<fc=#eee8d5,#dc322f> <fn=2>VPN</fn> </fc>    "
  fi
''
