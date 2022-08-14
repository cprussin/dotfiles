{
  pkgs,
  config,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  max_upload_size = "200M";
  synapse_port = 8008;
  federation_port = 8448;
  mautrix-telegram-port = 29317;
  mautrix-signal-port = 29328;
  homeserverUrl = "http://localhost:${toString synapse_port}";

  mautrix_settings = port: {
    appservice = {
      inherit port;
      address = "http://localhost:${toString port}";
    };
    homeserver = {
      address = homeserverUrl;
      domain = "prussin.net";
    };
    bridge = {
      permissions = {
        "prussin.net" = "full";
        "@connor:prussin.net" = "admin";
      };
      login_shared_secret_map."prussin.net" = "$SHARED_SECRET";
      encryption = {
        allow = true;
        default = true;
      };
    };
  };
in {
  deployment.keys = {
    "matrix-synapse-database-config.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseDatabaseConfigFile "Connor/Infrastructure/matrix/matrix-synapse-database";
    };
    "matrix-synapse-shared-secret-config.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseSharedSecretConfigFile "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    matrix-synapse-signing-key = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/matrix/signing-keys/prussin.net";
    };
    mautrix-telegram-environment-file = {
      user = "mautrix-telegram";
      group = "mautrix-telegram";
      keyCommand = passwords.getMautrixTelegramEnvironmentFile "Connor/Infrastructure/matrix/bridges/telegram" "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    "mautrix-telegram-registration-file.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixRegistrationFile "telegram" mautrix-telegram-port;
    };
    mautrix-signal-environment-file = {
      user = "mautrix-signal";
      group = "mautrix-signal";
      keyCommand = passwords.getMautrixSignalEnvironmentFile "Connor/Infrastructure/matrix/bridges/signal" "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    "mautrix-signal-registration-file.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixRegistrationFile "signal" mautrix-signal-port;
    };
    mautrix-syncproxy-environment-file = {
      user = "mautrix-syncproxy";
      group = "mautrix-syncproxy";
      keyCommand = passwords.getMautrixSyncproxyEnvironmentFile "Connor/Infrastructure/matrix/bridges/syncproxy";
    };
    mautrix-wsproxy-environment-file = {
      user = "mautrix-wsproxy";
      group = "mautrix-wsproxy";
      keyCommand = passwords.getMautrixWsproxyEnvironmentFile "Connor/Infrastructure/matrix/bridges/sms" "Connor/Infrastructure/matrix/bridges/syncproxy";
    };
    "mautrix-sms-registration-file.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixRegistrationFile "sms" 29331;
    };
  };

  services = {
    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."matrix.prussin.net" = {
        forceSSL = true;
        enableACME = true;
        http2 = true;
        listen = [
          {
            addr = "0.0.0.0";
            port = 443;
            ssl = true;
          }
          {
            addr = "[::]";
            port = 443;
            ssl = true;
          }
          {
            addr = "0.0.0.0";
            port = federation_port;
            ssl = true;
          }
          {
            addr = "[::]";
            port = federation_port;
            ssl = true;
          }
        ];
        locations."/" = {
          proxyPass = "http://localhost:${toString synapse_port}";
          extraConfig = "client_max_body_size ${max_upload_size};";
        };
      };
    };

    matrix-synapse = {
      enable = true;

      settings = {
        inherit max_upload_size;
        server_name = "prussin.net";
        signing_key_path = config.deployment.keys.matrix-synapse-signing-key.path;

        old_signing_keys = {
          "ed25519:a_OaaR" = {
            key = "ksE3M3GNPshFcrKYZXUWaMsTR9rtBgthcibsDpVGDK0";
            expired_ts = 1639995345267;
          };
        };

        listeners = [
          {
            port = synapse_port;
            bind_addresses = ["127.0.0.1"];
            tls = false;
            x_forwarded = true;
            resources = [
              {
                names = ["client"];
                compress = true;
              }
              {
                names = ["federation"];
                compress = false;
              }
            ];
          }
        ];

        app_service_config_files = [
          config.deployment.keys."mautrix-telegram-registration-file.yaml".path
          config.deployment.keys."mautrix-signal-registration-file.yaml".path
          config.deployment.keys."mautrix-sms-registration-file.yaml".path
        ];
      };

      plugins = [
        (config.services.matrix-synapse.package.plugins.matrix-synapse-shared-secret-auth.overrideAttrs (_: {
          version = "2.0.1";
          src = pkgs.fetchFromGitHub {
            owner = "devture";
            repo = "matrix-synapse-shared-secret-auth";
            rev = "2.0.1";
            sha256 = "0cbpj6npbnda23qrp7z5l33c95sh5mh21m9sc32xxiqaikj29ali";
          };
          buildInputs = [config.services.matrix-synapse.package];
        }))
      ];

      extraConfigFiles = [
        config.deployment.keys."matrix-synapse-database-config.yaml".path
        config.deployment.keys."matrix-synapse-shared-secret-config.yaml".path
      ];
    };

    mautrix-telegram = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-telegram-environment-file.path;
      settings = lib.recursiveUpdate (mautrix_settings mautrix-telegram-port) {
        bridge = {
          sync_create_limit = 0;
          sync_direct_chats = true;
          backfill.initial_limit = -1;
        };
      };
      serviceDependencies = ["postgresql.service" "mautrix-telegram-environment-file-key.service"];
    };

    mautrix-signal = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-signal-environment-file.path;
      settings = mautrix_settings mautrix-signal-port;
      serviceDependencies = ["postgresql.service" "mautrix-signal-environment-file-key.service"];
    };

    mautrix-wsproxy = {
      enable = true;
      secretsFile = config.deployment.keys.mautrix-wsproxy-environment-file.path;
    };

    mautrix-syncproxy = {
      inherit homeserverUrl;
      enable = true;
      secretsFile = config.deployment.keys.mautrix-syncproxy-environment-file.path;
    };

    postgresql.enable = true;
  };

  systemd.services = {
    matrix-synapse = {
      after = [
        "matrix-synapse-signing-key-key.service"
        "matrix-synapse-database-config.yaml-key.service"
        "matrix-synapse-shared-secret-config.yaml-key.service"
        "mautrix-telegram-registration-file.yaml-key.service"
        "mautrix-signal-registration-file.yaml-key.service"
        "mautrix-sms-registration-file.yaml-key.service"
      ];
      wants = [
        "matrix-synapse-signing-key-key.service"
        "matrix-synapse-database-config.yaml-key.service"
        "matrix-synapse-shared-secret-config.yaml-key.service"
        "mautrix-telegram-registration-file.yaml-key.service"
        "mautrix-signal-registration-file.yaml-key.service"
        "mautrix-sms-registration-file.yaml-key.service"
      ];
      serviceConfig.ExecStartPre = lib.mkForce [];
    };
    mautrix-telegram.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "mautrix-telegram";
      Group = "mautrix-telegram";
    };
    mautrix-signal.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "mautrix-signal";
      Group = "mautrix-signal";
    };
    mautrix-wsproxy = {
      after = ["mautrix-wsproxy-environment-file-key.service"];
      wants = ["mautrix-wsproxy-environment-file-key.service"];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "mautrix-wsproxy";
        Group = "mautrix-wsproxy";
      };
    };
    mautrix-syncproxy = {
      after = ["mautrix-syncproxy-environment-file-key.service"];
      wants = ["mautrix-syncproxy-environment-file-key.service"];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "mautrix-syncproxy";
        Group = "mautrix-syncproxy";
      };
    };
  };

  # see https://github.com/NixOS/nixpkgs/blob/nixos-21.11/nixos/modules/misc/ids.nix
  ids = {
    uids = {
      mautrix-telegram = 350;
      mautrix-signal = 351;
      mautrix-wsproxy = 352;
      mautrix-syncproxy = 353;
    };
    gids = {
      mautrix-telegram = 350;
      mautrix-signal = 351;
      mautrix-wsproxy = 352;
      mautrix-syncproxy = 353;
    };
  };

  users = {
    groups = {
      mautrix-telegram.gid = config.ids.gids.mautrix-telegram;
      mautrix-signal.gid = config.ids.gids.mautrix-signal;
      mautrix-wsproxy.gid = config.ids.gids.mautrix-wsproxy;
      mautrix-syncproxy.gid = config.ids.gids.mautrix-syncproxy;
    };
    users = {
      matrix-synapse.extraGroups = ["keys"];
      mautrix-telegram = {
        group = "mautrix-telegram";
        home = "/var/lib/mautrix-telegram";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-telegram;
        extraGroups = ["keys"];
      };
      mautrix-signal = {
        group = "mautrix-signal";
        home = "/var/lib/mautrix-signal";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-signal;
        extraGroups = ["keys" "signald"];
      };
      mautrix-wsproxy = {
        group = "mautrix-wsproxy";
        uid = config.ids.uids.mautrix-wsproxy;
        extraGroups = ["keys"];
      };
      mautrix-syncproxy = {
        group = "mautrix-syncproxy";
        uid = config.ids.uids.mautrix-syncproxy;
        extraGroups = ["keys"];
      };
    };
  };

  networking.firewall.allowedTCPPorts = [443 federation_port];
}
