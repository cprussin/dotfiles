{ pkgs, config, ... }:
let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
in
{
  deployment.keys = {
    library-basic-auth-file = {
      inherit (config.services.nginx) user group;
      keyCommand = passwords.getFullPassword "Infrastructure/htpasswd/library.prussin.net";
    };
  };

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    package = pkgs.nginxStable.override { modules = [ pkgs.nginxModules.fancyindex ]; };
    virtualHosts."library.prussin.net" = {
      forceSSL = true;
      enableACME = true;
      root = "/srv/Library";
      extraConfig = ''
        fancyindex on;
        fancyindex_exact_size off;
      '';
      basicAuthFile = config.deployment.keys.library-basic-auth-file.path;
    };
  };

  users.users.nginx.extraGroups = [ "keys" ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
