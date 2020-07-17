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
      permitRootLogin = "no";
      extraConfig = "PermitUserEnvironment yes";
    };

    security.pam = {
      enableSSHAgentAuth = true;
      services.sudo.sshAgentAuth = true;
    };

    systemd.services.sshd.wantedBy = lib.mkIf (!config.enableSshdAtBoot) (lib.mkForce []);

    primary-user.openssh.authorizedKeys.keys = [ passwords.public-ssh-key ];
  };
}
