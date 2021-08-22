{ config, lib, pkgs, ... }:
let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix { };

  allDevices = lib.genAttrs otherMachineNames (machine: {
    id = builtins.extraBuiltins.syncthingMachineId pkgs machine;
  });

  otherMachineNames =
    lib.remove config.networking.hostName (
      builtins.attrNames (builtins.readDir ../../../machines)
    );
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
      declarative = {
        cert = config.deployment.keys.syncthing-cert.path;
        key = config.deployment.keys.syncthing-key.path;
        devices = allDevices // {
          pegasus.id = "YDEJP2I-6H55ESK-NGMZ6OV-TWRVUCZ-WHQL2XA-SEDSDOY-Z7UWSJA-ZCVF3AH";
        };
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
    };

    systemd.services.syncthing = {
      wants = [
        "syncthing-cert-key.service"
        "syncthing-key-key.service"
      ];
      after = [
        "syncthing-cert-key.service"
        "syncthing-key-key.service"
      ];

      serviceConfig.ExecStartPost = pkgs.writeShellScript "rm-sync-dir" ''
        if [ -d "$HOME/Sync" ]
        then
          rmdir "$HOME/Sync"
        fi
      '';
    };

    users.users."${config.services.syncthing.user}".extraGroups = [ "keys" ];
  };
}
