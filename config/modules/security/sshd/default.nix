{ pkgs, lib, config, ... }:

let
  public-key = builtins.extraBuiltins.publicSshKey pkgs "connor@prussin.net";
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

    primary-user.openssh.authorizedKeys.keys = [ public-key ];
  };
}
