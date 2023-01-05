{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
  minFtpPort = 51000;
  maxFtpPort = 51999;
in {
  deployment.keys = {
    "library.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/library.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };
    "library.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/library.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [21 80 443];
    allowedTCPPortRanges = [
      {
        from = minFtpPort;
        to = maxFtpPort;
      }
    ];
  };

  users.users = {
    nginx.extraGroups = ["keys"];
    vsftpd.extraGroups = ["keys"];
  };

  services = {
    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      package = pkgs.nginxStable.override {modules = [pkgs.nginxModules.fancyindex];};
      virtualHosts."library.internal.prussin.net" = {
        listenAddresses = [network.wireguard.nodes.crux.address];
        sslCertificate = "/run/keys/library.internal.prussin.net.crt";
        sslCertificateKey = "/run/keys/library.internal.prussin.net.key";
        forceSSL = true;
        root = "/srv/Library";
        extraConfig = ''
          fancyindex on;
          fancyindex_exact_size off;
        '';
      };
    };

    vsftpd = {
      enable = true;
      anonymousUser = true;
      anonymousUserHome = "/srv/Library";
      anonymousUserNoPassword = true;
      rsaCertFile = "/run/keys/library.internal.prussin.net.crt";
      rsaKeyFile = "/run/keys/library.internal.prussin.net.key";
      extraConfig = ''
        allow_anon_ssl=YES
        ssl_ciphers=HIGH
        listen_ipv6=NO
        listen=YES
        listen_address=${network.wireguard.nodes.crux.address}
        pasv_enable=YES
        pasv_min_port=${toString minFtpPort}
        pasv_max_port=${toString maxFtpPort}
      '';
    };
  };

  systemd.services = {
    nginx = {
      after = [
        "library.internal.prussin.net.crt-key.service"
        "library.internal.prussin.net.key-key.service"
        "import-tank.service"
      ];
      requires = [
        "library.internal.prussin.net.crt-key.service"
        "library.internal.prussin.net.key-key.service"
        "import-tank.service"
      ];
    };

    vsftpd = {
      after = [
        "library.internal.prussin.net.crt-key.service"
        "library.internal.prussin.net.key-key.service"
        "import-tank.service"
      ];
      requires = [
        "library.internal.prussin.net.crt-key.service"
        "library.internal.prussin.net.key-key.service"
        "import-tank.service"
      ];
    };
  };
}
