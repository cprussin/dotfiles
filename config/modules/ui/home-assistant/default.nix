{ config, lib, ... }:

{
  options.services.home-assistant.virtualHost = lib.mkOption {
    type = lib.types.str;
  };

  config = {
    services = {
      nginx = {
        enable = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        virtualHosts."${config.services.home-assistant.virtualHost}" = {
          forceSSL = true;
          enableACME = true;
          extraConfig = "proxy_buffering off;";
          locations."/" = {
            proxyPass = "http://localhost:${toString config.services.home-assistant.port}";
            proxyWebsockets = true;
            extraConfig = "proxy_redirect http:// https://;";
          };
        };
      };

      home-assistant.enable = true;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
