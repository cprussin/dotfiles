{ writeScript, bash }:

# TODO use absolute paths here for systemctl and sudo...
writeScript "vpn" ''
  #! ${bash}/bin/sh

  if systemctl is-active --quiet openvpn-netflix; then
    sudo systemctl stop openvpn-netflix
  else
    sudo systemctl start openvpn-netflix
  fi
''
