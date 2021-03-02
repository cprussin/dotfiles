{ pkgs, config, ... }:
let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
in
{
  deployment.keys = {
    library-basic-auth-file = {
      inherit (config.services.nginx) user group;
      keyCommand = passwords.getFullPassword "Infrastructure/htpasswd/library.prussin.net";
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts."library.prussin.net" = {
      forceSSL = true;
      enableACME = true;
      root = "/srv/Library";
      extraConfig = "autoindex on;";
      basicAuthFile = config.deployment.keys.library-basic-auth-file.path;
    };
  };

  users.users.nginx.extraGroups = [ "keys" ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
