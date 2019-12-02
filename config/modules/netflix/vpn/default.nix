{ pkgs, config, ... }:

let
  user-pass = pkgs.writeText "user-pass" (
    builtins.concatStringsSep "\n" [
      (builtins.extraBuiltins.passwordField pkgs config "Netflix/Domain" "Username")
      (builtins.extraBuiltins.password pkgs config "Netflix/Domain")
    ]
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
      ca ${builtins.extraBuiltins.passwordFile pkgs config "Netflix/VPN/ca"}
      tls-auth ${builtins.extraBuiltins.passwordFile pkgs config "Netflix/VPN/tls-auth"}
      cert ${builtins.extraBuiltins.passwordFile pkgs config "Netflix/VPN/cert"}
      key ${builtins.extraBuiltins.passwordFile pkgs config "Netflix/VPN/key"}
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
