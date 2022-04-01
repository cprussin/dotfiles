{
  config,
  lib,
  ...
}: {
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

    networking.firewall.allowedTCPPorts = [80 443];

    users.users.hass.extraGroups = ["dialout"];

    # TODO Remove this once the home-assistant module is updated in nixpkgs from master (see https://github.com/NixOS/nixpkgs/commit/6267a995ec2abd5c4a9f977851e54ffaa7080977#diff-015e049c53b6e406aea53edfa01a770d8929ac8bfeadec133e15a22ccf25676e)
    systemd.services.home-assistant.serviceConfig.ExecStart = lib.mkForce "${config.services.home-assistant.package}/bin/hass --config '${config.services.home-assistant.configDir}'";
    systemd.services.home-assistant.serviceConfig.DeviceAllow = ["/dev/ttyACM0"];
  };
}
