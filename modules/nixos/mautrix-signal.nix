{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.services.mautrix-signal;
  settingsFormat = pkgs.formats.json {};
  settingsFileUnsubstituted = settingsFormat.generate "mautrix-signal-config-unsubstituted.json" cfg.settings;
in {
  options.services.mautrix-signal = {
    enable = lib.mkEnableOption "Mautrix-Signal a Matrix-Signal hybrid puppeting/relaybot bridge";

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/mautrix-signal";
    };

    registrationFile = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.dataDir}/signal-registration.yaml";
    };

    settingsFile = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.dataDir}/config.json";
    };

    settings = lib.mkOption rec {
      apply = lib.recursiveUpdate default;
      inherit (settingsFormat) type;
      default = {
        appservice = rec {
          database = "sqlite:///${cfg.dataDir}/mautrix-signal.db";
          database_opts = {};
          hostname = "0.0.0.0";
          port = 8080;
          address = "http://localhost:${toString port}";
        };

        signal.socket_path = config.services.signald.socketPath;

        bridge = {
          permissions."*" = "relaybot";
          relaybot.whitelist = [];
          double_puppet_server_map = {};
          login_shared_secret_map = {};
        };

        logging = {
          version = 1;

          formatters.precise.format = "[%(levelname)s@%(name)s] %(message)s";

          handlers.console = {
            class = "logging.StreamHandler";
            formatter = "precise";
          };

          loggers = {
            mau.level = "INFO";

            # prevent tokens from leaking in the logs:
            # https://github.com/tulir/mautrix-telegram/issues/351
            aiohttp.level = "WARNING";
          };

          # log to console/systemd instead of file
          root = {
            level = "INFO";
            handlers = ["console"];
          };
        };
      };
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        File containing environment variables to be passed to the mautrix-signal service,
        in which secret tokens can be specified securely by defining values for
        <literal>MAUTRIX_SIGNAL_APPSERVICE_AS_TOKEN</literal>, and
        <literal>MAUTRIX_SIGNAL_APPSERVICE_HS_TOKEN</literal>
      '';
    };

    serviceDependencies = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = lib.optional config.services.matrix-synapse.enable "matrix-synapse.service";
      defaultText = lib.literalExpression ''
        optional config.services.matrix-synapse.enable "matrix-synapse.service"
      '';
      description = ''
        List of Systemd services to require and wait for when starting the application service.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [
      (_: super: {
        mautrix-signal = super.mautrix-signal.overrideAttrs (attrs: {
          propagatedBuildInputs =
            attrs.propagatedBuildInputs
            ++ [
              pkgs.python3.pkgs.asyncpg
              pkgs.python3.pkgs.python-olm
              pkgs.python3.pkgs.pycryptodome
              pkgs.python3.pkgs.unpaddedbase64
            ];
        });
      })
    ];

    services.signald.enable = true;

    systemd.services.mautrix-signal = {
      description = "Mautrix-Signal, a Matrix-Signal hybrid puppeting/relaybot bridge.";

      wantedBy = ["multi-user.target"];
      wants = ["network-online.target"] ++ cfg.serviceDependencies;
      after = ["network-online.target"] ++ cfg.serviceDependencies;

      preStart =
        ''
          # Not all secrets can be passed as environment variable (yet)
          # https://github.com/tulir/mautrix-telegram/issues/584
          [ -f ${cfg.settingsFile} ] && rm -f ${cfg.settingsFile}
          old_umask=$(umask)
          umask 0177
          ${pkgs.envsubst}/bin/envsubst \
            -o ${cfg.settingsFile} \
            -i ${settingsFileUnsubstituted}
          umask $old_umask
          # generate the appservice's registration file if absent
          if [ ! -f '${cfg.registrationFile}' ]; then
            ${pkgs.mautrix-signal}/bin/mautrix-signal \
              --generate-registration \
              --base-config='${pkgs.mautrix-signal}/${pkgs.mautrix-signal.pythonModule.sitePackages}/mautrix_signal/example-config.yaml' \
              --config='${cfg.settingsFile}' \
              --registration='${cfg.registrationFile}'
          fi
        ''
        + lib.optionalString (pkgs.mautrix-signal ? alembic) ''
          # run automatic database init and migration scripts
          ${pkgs.mautrix-signal.alembic}/bin/alembic -x config='${cfg.settingsFile}' upgrade head
        '';

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
        WorkingDirectory = pkgs.mautrix-signal; # necessary for the database migration scripts to be found
        StateDirectory = baseNameOf cfg.dataDir;
        UMask = 0027;
        EnvironmentFile = cfg.environmentFile;

        ExecStart = ''
          ${pkgs.mautrix-signal}/bin/mautrix-signal \
            --config='${cfg.settingsFile}'
        '';
      };

      restartTriggers = [settingsFileUnsubstituted];
    };
  };
}
