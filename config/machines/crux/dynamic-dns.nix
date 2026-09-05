{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  # The v6 plugin is a clone of the v4 one, so it inherits a checkip of
  # Cloudflare's `1.1.1.1` while forcing its own traffic over v6 -- it has to
  # be told where to look instead.  A command rather than a `checkip-server`
  # because `get_address_backend` returns straight out of the command branch
  # (`src/ddns.c:463`), while the server branch falls back to plaintext
  # `http://ifconfig.me/ip` when it can't reach its server, and the connection
  # only prefers v6 rather than requiring it.  So a v6 outage under a
  # `checkip-server` ends with the v6 block holding a v4 address, and since the
  # record type is chosen from the address, it would then write that over the A
  # record and cache it as the AAAA.  A failing command just yields no address
  # and no update, which is what we want.
  #
  # `+time`/`+tries` because inadyn reads the command's output with an untimed
  # `popen` (`src/ddns.c:96`), so the only bound on a stalled lookup is
  # systemd's 90s start timeout -- which would fail the unit rather than skip
  # the cycle.  dig's own defaults stay under that, but not by much once the
  # lookup of the nameserver is counted too.
  public-ipv6 = pkgs.writeShellScript "public-ipv6" ''
    set -euo pipefail
    ${pkgs.dnsutils}/bin/dig -6 TXT +short +time=3 +tries=1 \
      o-o.myaddr.l.google.com @ns1.google.com \
      | ${pkgs.gnused}/bin/sed 's/"//g'
  '';

  # `username` is the zone, not a login -- the token in `include` is the whole
  # credential.
  #
  # `ttl` and `proxied` share one buffer in inadyn's cloudflare plugin and the
  # second write clobbers the first, so only `ttl` is set here.  Omitting
  # `proxied` leaves it alone rather than setting it false, since the update is
  # a PATCH -- which is fine, the record is unproxied and has to stay that way
  # because crux.prussin.net is the WireGuard endpoint.
  #
  # 60 is Cloudflare's floor for an explicit TTL; the alternative is its
  # "Auto", which is 300s.  Paired with the module's minutely timer that bounds
  # recovery from a WAN address change at about two minutes.
  cloudflare = {
    username = "prussin.net";
    hostname = "crux.prussin.net";
    ttl = 60;
    include = config.deployment.keys.inadyn-cloudflare-api-token.path;
  };
in {
  deployment.keys.inadyn-cloudflare-api-token = {
    inherit (config.users.users.inadyn) group;
    user = config.users.users.inadyn.name;
    keyCommand = passwords.getInadynSecrets "Connor/Infrastructure/cloudflare/dynamic-dns";
    destDir = "/secrets";
  };

  services.inadyn = {
    enable = true;
    settings = {
      # Without this a record edited by hand -- or one left at Auto by the
      # previous client -- is never corrected, since inadyn only writes on a
      # change it observes and never re-reads the record.  Daily is cheap next
      # to the minutely no-ops.
      forced-update = 86400;

      provider = {
        "default@cloudflare.com" = cloudflare;
        "ipv6@cloudflare.com" =
          cloudflare
          // {
            checkip-command = "${public-ipv6}";
          };
      };
    };
  };

  systemd.services.inadyn = {
    after = ["inadyn-cloudflare-api-token-key.service"];
    requires = ["inadyn-cloudflare-api-token-key.service"];
  };
}
