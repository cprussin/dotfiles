{src}: {
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.services.mautrix-syncproxy;
in {
  options.services.mautrix-syncproxy = {
    enable = lib.mkEnableOption "Mautrix Syncproxy";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      description = "The address where to listen.";
      default = ":29332";
    };

    homeserverUrl = lib.mkOption {
      type = lib.types.str;
      description = ''
        The address to Synapse. If using workers, it is sufficient to have
        access to the GET /sync and POST /user/{userId}/filter endpoints.
      '';
      default = "http://localhost:8008";
    };

    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = ''
        Path to a file containing the following:

        DATABASE_URL=<database conig: sqlite:///mautrix-syncproxy.db or postgres://user:pass@host/db>
        SHARED_SECRET=<The shared secret for adding new sync targets>
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [(import ./overlay.nix {inherit src;})];

    systemd.services.mautrix-syncproxy = {
      description = "Mautrix Syncproxy";

      wantedBy = ["multi-user.target"];
      wants = ["network-online.target" "matrix-synapse.service"];
      after = ["network-online.target" "matrix-synapse.service"];

      environment = {
        LISTEN_ADDRESS = cfg.listenAddress;
        HOMESERVER_URL = cfg.homeserverUrl;
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

        ExecStart = "${pkgs.mautrix-syncproxy}/bin/mautrix-syncproxy";
      };
    };
  };
}
