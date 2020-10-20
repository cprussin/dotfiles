{ config, lib, ... }:

{
  options.services.home-assistant.virtualHost = lib.mkOption {
    type = lib.types.str;
  };

  config = {
    services = {
      nginx = {
        enable = true;
        virtualHosts."${config.services.home-assistant.virtualHost}" = {
          forceSSL = true;
          enableACME = true;
          extraConfig = "proxy_buffering off;";
          locations."/".extraConfig = ''
            proxy_pass http://127.0.0.1:${toString config.services.home-assistant.port};
            proxy_set_header Host $host;
            proxy_redirect http:// https://;
            proxy_http_version 1.1;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection $connection_upgrade;
          '';
        };
      };

      home-assistant.enable = true;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
