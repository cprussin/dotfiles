{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    "private.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/private.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };
    "private.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/private.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  services.nginx.virtualHosts."private.internal.prussin.net" = {
    listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
    sslCertificate = config.deployment.keys."private.internal.prussin.net.crt".path;
    sslCertificateKey = config.deployment.keys."private.internal.prussin.net.key".path;
    forceSSL = true;
    http2 = true;
    locations."/" = {
      proxyPass = "http://localhost:42042";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };

  systemd.services.nginx = {
    requires = [
      "private.internal.prussin.net.crt-key.service"
      "private.internal.prussin.net.key-key.service"
    ];
    after = [
      "private.internal.prussin.net.crt-key.service"
      "private.internal.prussin.net.key-key.service"
    ];
  };

  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [42042];

  primary-user.home-manager.systemd.user.services.private = {
    Unit = {
      Description = "Private";
    };

    Service = {
      Type = "simple";
      ExecStart = "/bin/sh -c 'cd ~/Private && ${pkgs.nix}/bin/nix develop --command cli start-prod'";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = ["default.target"];
      Requires = ["import-tank.service"];
      After = ["import-tank.service"];
    };
  };
}
