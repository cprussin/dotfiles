{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in {
  deployment.keys.cloudflare-dyndns-api-token = {
    keyCommand = passwords.getPassword "Connor/Infrastructure/cloudflare/dynamic-dns";
    destDir = "/secrets";
  };

  services.cloudflare-dyndns = {
    enable = true;
    domains = ["crux.prussin.net"];
    apiTokenFile = config.deployment.keys.cloudflare-dyndns-api-token.path;
    ipv6 = true;
  };

  systemd.services.cloudflare-dyndns = {
    after = ["cloudflare-dyndns-api-token-key.service"];
    requires = ["cloudflare-dyndns-api-token-key.service"];
  };
}
