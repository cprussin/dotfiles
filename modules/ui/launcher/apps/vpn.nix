{ writeScript, bash, systemd }:

writeScript "vpn" ''
  #! ${bash}/bin/sh

  systemctl=${systemd}/bin/systemctl

  if $systemctl is-active --quiet openvpn-netflix
  then
    sudo $systemctl stop openvpn-netflix
  else
    sudo $systemctl start openvpn-netflix
  fi
''
