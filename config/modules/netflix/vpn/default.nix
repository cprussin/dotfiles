{ config, pkgs, lib, ... }:

let
  secret = name:
    pkgs.writeText name (
      builtins.extraBuiltins.pass pkgs config "Netflix/VPN/${name}"
    );
  user-pass-data = lib.splitString "\n" (
    builtins.extraBuiltins.pass pkgs config "Netflix/Domain"
  );
  username = lib.replaceStrings [ "Username: " ] [ "" ] (
    lib.findFirst (lib.hasPrefix "Username: ") "" user-pass-data
  );
  user-pass = pkgs.writeText "user-pass" ''
    ${username}
    ${builtins.elemAt user-pass-data 0}
  '';
in

{
  services.openvpn.servers.netflix = {
    config = ''
      client
      comp-lzo
      nobind
      persist-key
      persist-tun
      auth-nocache
      tun-mtu 1500
      remote lt.ovpn.netflix.net 443
      proto tcp
      mssfix
      route-method exe
      verb 3
      route-delay 2
      mute 20
      reneg-sec 0
      cipher AES-256-CBC
      auth SHA512
      key-direction 1
      dev-type tun
      dev tun
      auth-user-pass ${user-pass}
      ca ${secret "ca"}
      tls-auth ${secret "tls-auth"}
      cert ${secret "cert"}
      key ${secret "key"}
    '';
    autoStart = false;
    updateResolvConf = true;
  };

  primary-user.sudo-cmds = [
    "${pkgs.systemd}/bin/systemctl start openvpn-netflix"
    "${pkgs.systemd}/bin/systemctl stop openvpn-netflix"
    "${pkgs.systemd}/bin/systemctl restart openvpn-netflix"
  ];
}
