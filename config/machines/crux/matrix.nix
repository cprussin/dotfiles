{ pkgs, ... }:

let
  max_upload_size = "200M";
  synapse_port = 8008;
  federation_port = 8448;
in

{
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
      database_args.password = builtins.extraBuiltins.getPasswordValue pkgs "Infrastructure/postgresql/prussin.net/matrix-synapse";
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

  networking.firewall.allowedTCPPorts = [ 443 federation_port ];
}
