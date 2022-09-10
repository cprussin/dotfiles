{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    "library.internal.prussin.net.crt" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/library.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
      group = config.users.users.nginx.group;
    };
    "library.internal.prussin.net.key" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/library.internal.prussin.net/key";
      user = config.users.users.nginx.name;
      group = config.users.users.nginx.group;
    };
  };

  networking.firewall.allowedTCPPorts = [80 443];
  users.users.nginx.extraGroups = ["keys"];
  services.nginx = {
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

  systemd.services.nginx = {
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
}
