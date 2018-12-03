{ writeScript, bash, systemd, coreutils }:

writeScript "vpn" ''
  #! ${bash}/bin/sh

  echo=${coreutils}/bin/echo

  if ${systemd}/bin/systemctl is-active --quiet openvpn-netflix; then
      $echo "<fc=#eee8d5,#dc322f> <fn=2>VPN</fn> </fc>    "
  fi
''
