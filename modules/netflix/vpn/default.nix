{ lib, config, pkgs, ... }:

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
        auth-user-pass ${toString ../../data/nogit/secrets/netflix/netflix}
        ca ${toString ../../data/nogit/secrets/netflix/ca}
        tls-auth ${toString ../../data/nogit/secrets/netflix/tls-auth}
        cert ${toString ../../data/nogit/secrets/netflix/cert}
        key ${toString ../../data/nogit/secrets/netflix/key}
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
