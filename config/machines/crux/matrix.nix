{ pkgs, config, lib, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
  max_upload_size = "200M";
  synapse_port = 8008;
  federation_port = 8448;
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
    bridge.permissions = {
      "prussin.net" = "full";
      "@connor:prussin.net" = "admin";
    };
  };
in

{
  deployment.keys = {
    matrix-synapse-database-config = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseDatabaseConfigFile "Infrastructure/matrix/matrix-synapse-database";
    };
    matrix-synapse-signing-key = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getFullPassword "Infrastructure/matrix/signing-keys/prussin.net";
    };
    mautrix-telegram-environment-file = {
      user = "mautrix-telegram";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixTelegramEnvironmentFile "Infrastructure/matrix/bridges/Telegram";
    };
    mautrix-signal-environment-file = {
      user = "mautrix-signal";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixSignalEnvironmentFile "Infrastructure/matrix/bridges/Signal";
    };
    mautrix-syncproxy-environment-file = {
      user = "mautrix-syncproxy";
      group = "mautrix-syncproxy";
      keyCommand = passwords.getMautrixSyncproxyEnvironmentFile "Infrastructure/matrix/bridges/syncproxy";
    };
    mautrix-wsproxy-environment-file = {
      user = "mautrix-wsproxy";
      group = "mautrix-wsproxy";
      keyCommand = passwords.getMautrixWsproxyEnvironmentFile "Infrastructure/matrix/bridges/wsproxy" "Infrastructure/matrix/bridges/syncproxy";
    };
    mautrix-wsproxy-registration-file = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixWsproxyRegistrationFile "Infrastructure/matrix/bridges/wsproxy";
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
          { addr = "0.0.0.0"; port = 443; ssl = true; }
          { addr = "[::]"; port = 443; ssl = true; }
          { addr = "0.0.0.0"; port = federation_port; ssl = true; }
          { addr = "[::]"; port = federation_port; ssl = true; }
        ];
        locations."/" = {
          proxyPass = "http://localhost:${toString synapse_port}";
          extraConfig = "client_max_body_size ${max_upload_size};";
        };
      };
    };

    matrix-synapse = {
      inherit max_upload_size;

      server_name = "prussin.net";
      enable = true;
      extraConfigFiles = [
        config.deployment.keys.matrix-synapse-database-config.path
      ];
      extraConfig = ''
        signing_key_path: "${config.deployment.keys.matrix-synapse-signing-key.path}"
        old_signing_keys:
          "ed25519:a_OaaR": { key: "ksE3M3GNPshFcrKYZXUWaMsTR9rtBgthcibsDpVGDK0", expired_ts: 1639995345267 }
      '';
      listeners = [{
        port = synapse_port;
        bind_address = "127.0.0.1";
        tls = false;
        x_forwarded = true;
        resources = [
          { names = [ "client" ]; compress = true; }
          { names = [ "federation" ]; compress = false; }
        ];
      }];
      app_service_config_files = [
        "/var/lib/mautrix-telegram/telegram-registration.yaml"
        config.services.mautrix-signal.registrationFile
        config.deployment.keys.mautrix-wsproxy-registration-file.path
      ];
    };

    mautrix-telegram = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-telegram-environment-file.path;
      settings = mautrix_settings 29317;
      serviceDependencies = [ "postgresql.service" "mautrix-telegram-environment-file-key.service" ];
    };

    mautrix-signal = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-signal-environment-file.path;
      settings = mautrix_settings 29328;
      serviceDependencies = [ "postgresql.service" "mautrix-signal-environment-file-key.service" ];
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
      after = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" "mautrix-wsproxy-registration-file-key.service" ];
      wants = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" "mautrix-wsproxy-registration-file-key.service" ];
      serviceConfig.ExecStartPre = lib.mkForce [ ];
    };
    mautrix-telegram.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "mautrix-telegram";
      Group = "matrix-synapse";
    };
    mautrix-signal.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "mautrix-signal";
      Group = "matrix-synapse";
    };
    mautrix-wsproxy = {
      after = [ "mautrix-wsproxy-environment-file-key.service" ];
      wants = [ "mautrix-wsproxy-environment-file-key.service" ];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "mautrix-wsproxy";
        Group = "mautrix-wsproxy";
      };
    };
    mautrix-syncproxy = {
      after = [ "mautrix-syncproxy-environment-file-key.service" ];
      wants = [ "mautrix-syncproxy-environment-file-key.service" ];
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
      mautrix-wsproxy = 352;
      mautrix-syncproxy = 353;
    };
  };

  users = {
    groups = {
      mautrix-wsproxy.gid = config.ids.gids.mautrix-wsproxy;
      mautrix-syncproxy.gid = config.ids.gids.mautrix-syncproxy;
    };
    users = {
      matrix-synapse.extraGroups = [ "keys" ];
      mautrix-telegram = {
        group = "matrix-synapse";
        home = "/var/lib/mautrix-telegram";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-telegram;
        extraGroups = [ "keys" ];
      };
      mautrix-signal = {
        group = "matrix-synapse";
        home = "/var/lib/mautrix-signal";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-signal;
        extraGroups = [ "keys" "signald" ];
      };
      mautrix-wsproxy = {
        group = "mautrix-wsproxy";
        uid = config.ids.uids.mautrix-wsproxy;
        extraGroups = [ "keys" ];
      };
      mautrix-syncproxy = {
        group = "mautrix-syncproxy";
        uid = config.ids.uids.mautrix-syncproxy;
        extraGroups = [ "keys" ];
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 443 federation_port ];
}
