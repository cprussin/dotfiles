# ntfy is the offer adder's escape hatch: when Amex or Chase insists on a
# one-time code, the run pushes a notification here and waits for a phone to
# publish the digits back. It is wireguard-only and its whole auth database --
# users, ACLs and the service account's token -- is provisioned from pass at
# start, so nothing about it is imperative.
{
  config,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
  host = "ntfy.internal.prussin.net";
  # Loopback only: nginx terminates TLS for the phones, and the offer adder
  # running on this same box talks to it directly, which keeps the internal CA
  # out of bun's certificate store entirely.
  listenAddress = "127.0.0.1:2586";
in {
  deployment.keys = {
    ntfy-secrets = {
      keyCommand =
        passwords.getNtfySecrets "offers-*"
        "Connor/Infrastructure/ntfy/offer-adder"
        [
          "Connor/Infrastructure/ntfy/connor"
          "Shauna/Infrastructure/ntfy/shauna"
        ];
      destDir = "/secrets";
    };

    "${host}.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/${host}/cert";
      user = config.users.users.nginx.name;
    };

    "${host}.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/${host}/key";
      user = config.users.users.nginx.name;
    };
  };

  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [80 443];

  services = {
    ntfy-sh = {
      enable = true;
      environmentFile = config.deployment.keys.ntfy-secrets.path;
      settings = {
        base-url = "https://${host}";
        listen-http = listenAddress;
        behind-proxy = true;
        # Every topic is private; the ACLs in the environment file are the only
        # way in.  The auth database is rebuilt from that file on every start,
        # so its StateDirectory is deliberately left on the tmpfs root: nothing
        # in it needs to survive a reboot except what the environment already
        # carries.  The one consequence is that a token minted at runtime
        # through /v1/account/token does not come back, so the phones should
        # authenticate with their username and password.
        auth-default-access = "deny-all";
      };
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."${host}" = {
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
        sslCertificate = config.deployment.keys."${host}.crt".path;
        sslCertificateKey = config.deployment.keys."${host}.key".path;
        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://${listenAddress}";
          proxyWebsockets = true;
          # Subscribers hold a stream open until something is published, so
          # buffering it would defeat the point entirely.  The read timeout is
          # only belt and braces -- ntfy keepalives every 45s, inside nginx's
          # 60s default -- but it is set past the five minutes a run waits for
          # a code, so a phone that misses a keepalive still holds the stream
          # across a whole request.
          extraConfig = ''
            proxy_buffering off;
            proxy_request_buffering off;
            proxy_read_timeout 6m;
          '';
        };
      };
    };
  };

  systemd.services = {
    ntfy-sh = {
      after = ["ntfy-secrets-key.service"];
      requires = ["ntfy-secrets-key.service"];
    };

    nginx = {
      after = [
        "${host}.crt-key.service"
        "${host}.key-key.service"
      ];
      requires = [
        "${host}.crt-key.service"
        "${host}.key-key.service"
      ];
    };
  };
}
