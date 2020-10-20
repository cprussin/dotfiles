{ pkgs, config, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in

{
  deployment.keys = {
    route53-dynamic-dns-aws-access-key.text = passwords.get-password-field "Computer Services/AWS/crux dynamic dns" "Access Key Id";
    route53-dynamic-dns-aws-secret-access-key.text = passwords.get-password-field "Computer Services/AWS/crux dynamic dns" "Secret Access Key";
  };

  services.route53DynamicDns = {
    enable = true;
    zoneId = "ZPN01N69TQ4DV";
    cname = "crux.prussin.net";
    accessKeyFile = config.deployment.keys.route53-dynamic-dns-aws-access-key.path;
    secretAccessKeyFile = config.deployment.keys.route53-dynamic-dns-aws-secret-access-key.path;
  };

  systemd.services.route53-dynamic-dns-update = {
    after = [
      "route53-dynamic-dns-aws-access-key-key.service"
      "route53-dynamic-dns-aws-secret-access-key-key.service"
    ];
    wants = [
      "route53-dynamic-dns-aws-access-key-key.service"
      "route53-dynamic-dns-aws-secret-access-key-key.service"
    ];
  };
}
