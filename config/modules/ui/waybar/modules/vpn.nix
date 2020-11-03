{ pkgs, colors }:
let
  vpn = pkgs.writeShellScript "vpn" ''
    if systemctl is-active --quiet openvpn-netflix
    then
      echo VPN
    fi
  '';
in
{
  name = "custom/vpn";

  config = {
    exec = vpn;
    interval = 1;
  };

  style."" = {
    background-color = colors.red;
    color = colors.white;
    font-weight = "bold";
  };
}
