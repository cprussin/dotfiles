{ pkgs, config, lib, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
  max_upload_size = "200M";
  synapse_port = 8008;
  federation_port = 8448;

  mautrix_settings = port: {
    appservice = {
      inherit port;
      address = "http://localhost:${toString port}";
    };
    homeserver = {
      address = "http://localhost:${toString synapse_port}";
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

    postgresql.enable = true;
  };

  systemd.services = {
    matrix-synapse = {
      after = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" ];
      wants = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" ];
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
  };

  # see https://github.com/NixOS/nixpkgs/blob/nixos-21.11/nixos/modules/misc/ids.nix
  ids = {
    uids.mautrix-telegram = 350;
    uids.mautrix-signal = 351;
  };

  users.users = {
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
  };

  networking.firewall.allowedTCPPorts = [ 443 federation_port ];
}
