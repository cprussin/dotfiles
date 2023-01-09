{
  pkgs,
  lib,
  config,
  ...
}: let
  # TODO this should really be driven from gpg automatically rather than manually synced to a file...
  public-key = builtins.readFile ./public-key;
  gaming-computer-wsl-public-key = builtins.readFile ./gaming-computer-wsl-public-key;
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
in {
  options = {
    enableSshdAtBoot = lib.mkEnableOption "SSH daemon auto-start at boot";
    enableSshdAtInitrd = lib.mkEnableOption "SSH daemon startup in initial ramdisk";
  };

  config = {
    deployment.keys = {
      sshHostED25519Key = {
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/hostKeys/ed25519/${config.networking.hostName}";
        destDir = "/secrets";
      };
      sshHostRSAKey = {
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/hostKeys/rsa/${config.networking.hostName}";
        destDir = "/secrets";
      };
      bootSshHostED25519Key = lib.mkIf config.enableSshdAtInitrd {
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/hostKeys/ed25519/${config.networking.hostName}-initrd";
        destDir = "/secrets";
      };
      bootSshHostRSAKey = lib.mkIf config.enableSshdAtInitrd {
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/hostKeys/rsa/${config.networking.hostName}-initrd";
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
          inherit (config.deployment.keys.sshHostED25519Key) path;
          type = "ed25519";
        }
        {
          inherit (config.deployment.keys.sshHostRSAKey) path;
          type = "rsa";
          bits = 4096;
        }
      ];
    };

    security.pam = {
      enableSSHAgentAuth = true;
      services.sudo.sshAgentAuth = true;
    };

    systemd.services.sshd.wantedBy = lib.mkIf (!config.enableSshdAtBoot) (lib.mkForce []);

    primary-user.openssh.authorizedKeys.keys = [
      public-key
      gaming-computer-wsl-public-key
    ];

    boot.initrd.network = lib.mkIf config.enableSshdAtInitrd {
      enable = true;
      ssh = {
        enable = true;
        hostKeys = [
          config.deployment.keys.bootSshHostED25519Key.path
          config.deployment.keys.bootSshHostRSAKey.path
        ];
        authorizedKeys = [public-key];
      };
    };
  };
}
