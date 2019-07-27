{ lib, config, pkgs, ... }:

let
  secret = name: pkgs.writeText name (builtins.extraBuiltins.pass pkgs ("Netflix/VPN/" + name));
in

{
  config = {
    services.openvpn.servers.netflix = {
      config = ''
        client
        comp-lzo
        nobind
        persist-key
        persist-tun
        auth-nocache
        dev tun
        tun-mtu 1500
        remote Aviatrix-vpc-f73da2920xd38c64b-562622904.us-west-1.elb.amazonaws.com 443
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
        auth-user-pass ${secret "user-pass"}
        ca ${secret "ca"}
        tls-auth ${secret "tls-auth"}
        cert ${secret "cert"}
        key ${secret "key"}
      '';
      autoStart = false;
      updateResolvConf = true;
    };

    sudoCmds = [
      "${pkgs.systemd}/bin/systemctl start openvpn-netflix"
      "${pkgs.systemd}/bin/systemctl stop openvpn-netflix"
      "${pkgs.systemd}/bin/systemctl restart openvpn-netflix"
    ];
  };
}
