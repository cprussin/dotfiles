{ pkgs, config, ... }:
let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix { };
in
{
  deployment.keys = {
    netflix-vpn-user-pass = {
      text = builtins.concatStringsSep "\n" [
        (passwords.get-password-field "Netflix/Domain" "Username")
        (passwords.get-password "Netflix/Domain")
      ];
      destDir = "/secrets";
    };

    netflix-vpn-ca = {
      text = passwords.get-full-password "Netflix/VPN/ca";
      destDir = "/secrets";
    };

    netflix-vpn-tls-auth = {
      text = passwords.get-full-password "Netflix/VPN/tls-auth";
      destDir = "/secrets";
    };

    netflix-vpn-cert = {
      text = passwords.get-full-password "Netflix/VPN/cert";
      destDir = "/secrets";
    };

    netflix-vpn-key = {
      text = passwords.get-full-password "Netflix/VPN/key";
      destDir = "/secrets";
    };
  };

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
      auth-user-pass ${config.deployment.keys.netflix-vpn-user-pass.path}
      ca ${config.deployment.keys.netflix-vpn-ca.path}
      tls-auth ${config.deployment.keys.netflix-vpn-tls-auth.path}
      cert ${config.deployment.keys.netflix-vpn-cert.path}
      key ${config.deployment.keys.netflix-vpn-key.path}
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
