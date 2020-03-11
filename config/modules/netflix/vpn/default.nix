{ pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};

  user-pass = pkgs.writeText "user-pass" (
    builtins.concatStringsSep "\n" [
      (passwords.get-password-field "Netflix/Domain" "Username")
      (passwords.get-password "Netflix/Domain")
    ]
  );

  write-secret-file = pass:
    pkgs.writeText (builtins.replaceStrings [ "/" ] [ "-" ] pass) (
      passwords.get-password-field pass "Full"
    );
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
      ca ${write-secret-file "Netflix/VPN/ca"}
      tls-auth ${write-secret-file "Netflix/VPN/tls-auth"}
      cert ${write-secret-file "Netflix/VPN/cert"}
      key ${write-secret-file "Netflix/VPN/key"}
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
