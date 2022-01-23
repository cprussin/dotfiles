{ pkgs, config, lib, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
  max_upload_size = "200M";
  synapse_port = 8008;
  federation_port = 8448;
in

{
  users.users.matrix-synapse.extraGroups = [ "keys" ];

  deployment.keys = {
    matrix-synapse-database-config = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseDatabaseConfigFile "Infrastructure/postgresql/prussin.net/matrix-synapse";
    };
    matrix-synapse-signing-key = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getFullPassword "Infrastructure/matrix/signing-keys/prussin.net";
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
    };

    postgresql.enable = true;
  };

  systemd.services.matrix-synapse = {
    after = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" ];
    wants = [ "matrix-synapse-signing-key-key.service" "matrix-synapse-database-config-key.service" ];
    serviceConfig.ExecStartPre = lib.mkForce [ ];
  };

  networking.firewall.allowedTCPPorts = [ 443 federation_port ];
}
