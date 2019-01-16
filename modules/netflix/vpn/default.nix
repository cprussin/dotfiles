{ lib, config, pkgs, ... }:

{
  options.netflixVpnAuthPath = lib.mkOption {
    type = lib.types.str;
    default = "/home/${config.primaryUserName}/Projects/dotfiles/modules/data/nogit/secrets/netflix";
    description = ''
      A string containing the path containing the Netflix VPN authentication
      files.  This directory should contain the following files:

      - `netflix`: an auth-user-pass file
      - `ca`
      - `tls-auth`
      - `cert`
      - `key`
    '';
  };

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
        auth-user-pass ${config.netflixVpnAuthPath}/netflix
        ca ${config.netflixVpnAuthPath}/ca
        tls-auth ${config.netflixVpnAuthPath}/tls-auth
        cert ${config.netflixVpnAuthPath}/cert
        key ${config.netflixVpnAuthPath}/key
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
