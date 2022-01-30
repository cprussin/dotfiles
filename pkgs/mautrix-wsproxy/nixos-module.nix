{ src }: { lib, config, pkgs, ... }:

let
  cfg = config.services.mautrix-wsproxy;
  port = 29331;
in

{
  options.services.mautrix-wsproxy = {
    enable = lib.mkEnableOption "Mautrix Wsproxy";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      description = "The address where to listen.";
      default = ":${toString port}";
    };

    appserviceId = lib.mkOption {
      type = lib.types.str;
      description = "The id of the appservice.";
      default = "mautrix-wsproxy";
    };

    syncProxyUrl = lib.mkOption {
      type = lib.types.str;
      description = "The url of the syncproxy instance.";
      default = "http://localhost:29332";
    };

    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = ''
        Path to a file containing the following:

        AS_TOKEN=<A randomly generated AS token>
        HS_TOKEN=<A randomly generated HS token>
        SYNC_PROXY_SHARED_SECRET=<The shared secret for adding new sync targets>
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ (import ./overlay.nix { inherit src; }) ];

    systemd.services.mautrix-wsproxy = {
      description = "Mautrix Wsproxy";

      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" "matrix-synapse.service" ];
      after = [ "network-online.target" "matrix-synapse.service" ];

      environment = {
        LISTEN_ADDRESS = cfg.listenAddress;
        APPSERVICE_ID = cfg.appserviceId;
        SYNC_PROXY_URL = cfg.syncProxyUrl;
        SYNC_PROXY_WSPROXY_URL = "http://localhost:${toString port}";
      };

      serviceConfig = {
        Type = "simple";
        Restart = "always";

        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;

        DynamicUser = true;
        PrivateTmp = true;
        EnvironmentFile = cfg.secretsFile;

        ExecStart = "${pkgs.mautrix-wsproxy}/bin/mautrix-wsproxy -config env";
      };
    };

    networking.firewall.allowedTCPPorts = [ port ];
  };
}
