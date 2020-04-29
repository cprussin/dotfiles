{ pkgs, lib, config, ... }:

let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
in

{
  options.enableSshdAtBoot = lib.mkEnableOption "SSH daemon auto-start at boot";

  config = {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      extraConfig = "PermitUserEnvironment yes";
    };

    systemd.services.sshd.wantedBy = lib.mkIf (!config.enableSshdAtBoot) (lib.mkForce []);

    primary-user.openssh.authorizedKeys.keys = [ passwords.public-ssh-key ];
  };
}
