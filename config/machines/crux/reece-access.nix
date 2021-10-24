{ config, pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
  reece-public-key = builtins.readFile ./reece-public-key;
in

{
  deployment.keys.reece-password = {
    keyCommand = passwords.getHashedUserPassword "Infrastructure/login/reece@${config.networking.hostName}";
    destDir = "/secrets";
  };

  users.users.reece = {
    isNormalUser = true;
    passwordFile = config.deployment.keys.reece-password.path;
    openssh.authorizedKeys.keys = [ reece-public-key ];
  };
}
