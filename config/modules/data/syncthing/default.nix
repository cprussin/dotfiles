{ config, lib, pkgs, ... }:
let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix { };

  otherMachineNames =
    lib.remove config.networking.hostName (
      builtins.attrNames (builtins.readDir ../../../machines)
    );

  # TODO this should really be driven from the cert files in the password store
  # automatically rather than manually copied here
  syncthingMachineIds = {
    gemini = "SNVES23-LNUWRQA-EYZW3SB-NGPXW5F-5OYCJ2N-J43WSKW-PXX2KAI-ZJ3ZUQ7";
    orion = "YCU5GWY-Y6IAQM6-VU6JDKI-IQJX66P-REDNV4Z-JOQTMUD-DITN3XQ-MWV62A2";
    crux = "I4ZIHKH-5UQLYN3-I6TGPKJ-IXVKJYK-NTO6RQG-QPIFI4Y-NMC3IHO-4OPHDAM";
    pegasus = "YDEJP2I-6H55ESK-NGMZ6OV-TWRVUCZ-WHQL2XA-SEDSDOY-Z7UWSJA-ZCVF3AH";
  };
in
{
  options.persistSyncthingKeys = lib.mkEnableOption "Persist syncthing keys across reboots";

  config = {
    deployment.keys = {
      syncthing-cert = {
        inherit (config.services.syncthing) user group;
        destDir = lib.mkIf config.persistSyncthingKeys "/secrets";
        keyCommand = passwords.getFullPassword "Infrastructure/syncthing/${config.networking.hostName}/cert";
      };

      syncthing-key = {
        inherit (config.services.syncthing) user group;
        destDir = lib.mkIf config.persistSyncthingKeys "/secrets";
        keyCommand = passwords.getFullPassword "Infrastructure/syncthing/${config.networking.hostName}/key";
      };
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = config.primary-user.name;
      cert = config.deployment.keys.syncthing-cert.path;
      key = config.deployment.keys.syncthing-key.path;
      devices = lib.genAttrs (otherMachineNames ++ [ "pegasus" ]) (machine: {
        id = syncthingMachineIds."${machine}";
      });
      folders = {
        Notes = {
          path = "${config.primary-user.home}/Notes";
          devices = lib.remove config.networking.hostName [ "pegasus" "crux" "gemini" "orion" ];
        };
        Projects = {
          path = "${config.primary-user.home}/Projects";
          devices = lib.remove config.networking.hostName [ "crux" "gemini" "orion" ];
        };
        Scratch = {
          path = "${config.primary-user.home}/Scratch";
          devices = lib.remove config.networking.hostName [ "pegasus" "crux" "gemini" "orion" ];
        };
      };
    };

    systemd.services = {
      syncthing = {
        wants = [
          "syncthing-cert-key.service"
          "syncthing-key-key.service"
        ];
        after = [
          "syncthing-cert-key.service"
          "syncthing-key-key.service"
        ];
      };

      syncthing-init.serviceConfig.ExecStartPost = pkgs.writeShellScript "rm-sync-dir" ''
        if [ -d "$HOME/Sync" ]
        then
          rmdir "$HOME/Sync"
        fi
      '';
    };

    users.users."${config.services.syncthing.user}".extraGroups = [ "keys" ];
  };
}
