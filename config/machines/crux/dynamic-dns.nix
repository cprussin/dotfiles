{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  # crux's address is what every WireGuard peer and inbound service resolves,
  # so the record wants a short TTL -- but cloudflare-dyndns publishes
  # Cloudflare's "Auto" (~300s) and exposes no knob for it.  Neither does any
  # other DDNS client packaged in nixpkgs: ddclient's `cloudflare` protocol
  # declares no `ttl` config var, and while inadyn's does, its cache file is
  # keyed on the plugin name, so the A and AAAA provider blocks would share one
  # cache and rewrite each other's record every run.  Patching this one is the
  # smaller lie.
  cloudflare-dyndns = pkgs.cloudflare-dyndns.overrideAttrs (old: {
    patches = (old.patches or []) ++ [./cloudflare-dyndns-ttl.patch];
  });
in {
  deployment.keys.cloudflare-dyndns-api-token = {
    keyCommand = passwords.getPassword "Connor/Infrastructure/cloudflare/dynamic-dns";
    destDir = "/secrets";
  };

  services.cloudflare-dyndns = {
    enable = true;
    package = cloudflare-dyndns;
    domains = ["crux.prussin.net"];
    apiTokenFile = config.deployment.keys.cloudflare-dyndns-api-token.path;
    # Paired with the 60s TTL above, this bounds a move at about two minutes.
    # It costs two requests a minute to api.ipify.org: the address lookup runs
    # before the cache comparison, so it happens whether or not the IP moved.
    frequency = "minutely";
    ipv6 = true;
  };

  systemd.services.cloudflare-dyndns = {
    after = ["cloudflare-dyndns-api-token-key.service"];
    requires = ["cloudflare-dyndns-api-token-key.service"];
  };
}
