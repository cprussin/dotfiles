{ pkgs, lib, config, ... }:
let
  public-key = builtins.extraBuiltins.publicSshKey pkgs "connor@prussin.net";
  passwords = pkgs.callPackage ../../../../lib/passwords.nix { };
in
{
  options.enableSshdAtBoot = lib.mkEnableOption "SSH daemon auto-start at boot";

  config = {
    deployment.keys = {
      sshHostED25519Key = {
        text = passwords.get-full-password "Infrastructure/ssh/hostKeys/ed25519/${config.networking.hostName}";
        destDir = "/secrets";
      };
      sshHostRSAKey = {
        text = passwords.get-full-password "Infrastructure/ssh/hostKeys/rsa/${config.networking.hostName}";
        destDir = "/secrets";
      };
    };

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
      extraConfig = "PermitUserEnvironment yes";

      hostKeys = [
        {
          path = config.deployment.keys.sshHostED25519Key.path;
          type = "ed25519";
        }
        {
          path = config.deployment.keys.sshHostRSAKey.path;
          type = "rsa";
          bits = 4096;
        }
      ];
    };

    security.pam = {
      enableSSHAgentAuth = true;
      services.sudo.sshAgentAuth = true;
    };

    systemd.services.sshd.wantedBy = lib.mkIf (!config.enableSshdAtBoot) (lib.mkForce [ ]);

    primary-user.openssh.authorizedKeys.keys = [ public-key ];
  };
}
