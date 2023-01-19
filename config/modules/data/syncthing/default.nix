{
  config,
  lib,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};

  otherMachineNames = lib.remove config.networking.hostName (
    builtins.attrNames (builtins.readDir ../../../machines)
  );

  # TODO this should really be driven from the cert files in the password store
  # automatically rather than manually copied here
  syncthingMachineIds = {
    gemini = "SNVES23-LNUWRQA-EYZW3SB-NGPXW5F-5OYCJ2N-J43WSKW-PXX2KAI-ZJ3ZUQ7";
    crux = "I4ZIHKH-5UQLYN3-I6TGPKJ-IXVKJYK-NTO6RQG-QPIFI4Y-NMC3IHO-4OPHDAM";
    pegasus = "2R6PRKY-KL77MPG-NJELBK7-H6ER2NM-CQRBWKQ-CAMS45M-FFVKWZA-LBKJXA4";
  };

  includesCurrentDevice = lib.any (dev: dev == config.networking.hostName);
  removeCurrentDeviceFromFolder = folder:
    folder
    // {
      devices = lib.remove config.networking.hostName folder.devices;
    };

  foldersForCurrentDevice = folders:
    builtins.mapAttrs (_: removeCurrentDeviceFromFolder) (
      lib.filterAttrs
      (_: folder: includesCurrentDevice folder.devices)
      folders
    );
in {
  options.persistSyncthingKeys = lib.mkEnableOption "Persist syncthing keys across reboots";

  config = {
    deployment.keys = {
      syncthing-cert = {
        inherit (config.services.syncthing) user group;
        destDir = lib.mkIf config.persistSyncthingKeys "/secrets";
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/syncthing/${config.networking.hostName}/cert";
      };

      syncthing-key = {
        inherit (config.services.syncthing) user group;
        destDir = lib.mkIf config.persistSyncthingKeys "/secrets";
        keyCommand = passwords.getFullPassword "Connor/Infrastructure/syncthing/${config.networking.hostName}/key";
      };
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = config.primary-user.name;
      cert = config.deployment.keys.syncthing-cert.path;
      key = config.deployment.keys.syncthing-key.path;
      devices = lib.genAttrs (otherMachineNames ++ ["pegasus"]) (machine: {
        id = syncthingMachineIds."${machine}";
      });
      folders = foldersForCurrentDevice {
        Notes = {
          path = "${config.primary-user.home}/Notes";
          devices = ["crux" "gemini" "pegasus"];
        };
        Projects = {
          path = "${config.primary-user.home}/Projects";
          devices = ["crux" "gemini"];
        };
        Scratch = {
          path = "${config.primary-user.home}/Scratch";
          devices = ["crux" "gemini" "pegasus"];
        };
        DCIM = {
          path = "${config.primary-user.home}/Phone/DCIM";
          devices = ["crux" "pegasus"];
        };
        Pictures = {
          path = "${config.primary-user.home}/Phone/Pictures";
          devices = ["crux" "pegasus"];
        };
        WhatsApp_Media = {
          path = "${config.primary-user.home}/Phone/WhatsApp Media";
          devices = ["crux" "pegasus"];
        };
        SMS_Backup = {
          path = "${config.primary-user.home}/Phone/SMS";
          devices = ["crux" "pegasus"];
        };
        Game_Boy_Saves = {
          path = "${config.primary-user.home}/Game Saves/Game Boy";
          devices = ["crux"];
        };
        Game_Boy_Color_Saves = {
          path = "${config.primary-user.home}/Game Saves/Game Boy Color";
          devices = ["crux"];
        };
        Game_Boy_Advance_Saves = {
          path = "${config.primary-user.home}/Game Saves/Game Boy Advance";
          devices = ["crux"];
        };
        Super_Nintendo_Saves = {
          path = "${config.primary-user.home}/Game Saves/Super Nintendo";
          devices = ["crux"];
        };
        GameCube_Saves = {
          path = "${config.primary-user.home}/Game Saves/GameCube";
          devices = ["crux"];
        };
        Stardew_Valley_Saves = {
          path =
            if config.networking.hostName == "crux"
            then "${config.primary-user.home}/Game Saves/Stardew Valley"
            else "${config.primary-user.home}/.config/StardewValley/Saves";
          devices = ["crux" "gemini" "pegasus"];
        };
        Factorio_Saves = {
          path =
            if config.networking.hostName == "crux"
            then "${config.primary-user.home}/Game Saves/Factorio"
            else "${config.primary-user.home}/.factorio/saves";
          devices = ["crux" "gemini"];
        };
      };
    };

    systemd.services = {
      syncthing = {
        requires = [
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

    users.users."${config.services.syncthing.user}".extraGroups = ["keys"];
  };
}
