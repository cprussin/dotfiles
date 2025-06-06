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

  networking.firewall.interfaces.prussinnet = {
    allowedTCPPorts = [21 80 443 2049];
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
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
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
        listen=NO
        listen_ipv6=YES
        listen_address=${network.wireguard4.crux.address}
        listen_address6=${network.wireguard6.crux.address}
        pasv_enable=YES
        pasv_min_port=${toString minFtpPort}
        pasv_max_port=${toString maxFtpPort}
      '';
    };

    nfs.server = {
      enable = true;
      hostName = "${network.wireguard6.crux.address},${network.wireguard4.crux.address}";
      exports = ''
        /srv/Library *(ro,fsid=0,no_subtree_check,crossmnt,insecure,all_squash)
        "/srv/Library/Family Photos" *(rw,no_subtree_check,insecure,all_squash)
        "/srv/Library/Documents" *(rw,no_subtree_check,insecure,all_squash)
        "/srv/Library/Google Drive" *(rw,no_subtree_check,insecure,all_squash)
        "/srv/Library/TheJobFare" *(rw,no_subtree_check,insecure,all_squash)
        "/srv/Library/Lego Instructions" *(rw,no_subtree_check,insecure,all_squash)
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
