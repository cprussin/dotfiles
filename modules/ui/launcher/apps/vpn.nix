{ writeShellScript, systemd }:

writeShellScript "vpn" ''
  systemctl=${systemd}/bin/systemctl

  if $systemctl is-active --quiet openvpn-netflix
  then
    sudo $systemctl stop openvpn-netflix
  else
    sudo $systemctl start openvpn-netflix
  fi
''
