{
  config,
  lib,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../../lib/network.nix {};

  # TODO this should really be driven from the cert files in the password store
  # automatically rather than manually copied here
  syncthingMachineIds = {
    lyra = "LFXISO5-EXAHN36-VIQRVBB-JTX5MZO-CQG22QS-VIKCZ54-Z5LMM3N-Z3HKBQB";
    crux = "I4ZIHKH-5UQLYN3-I6TGPKJ-IXVKJYK-NTO6RQG-QPIFI4Y-NMC3IHO-4OPHDAM";
    pegasus = "2JXRBGP-OHUREJQ-YFNLX4T-7XW52VO-3N3IKBF-G3QZJ2C-4FBCGDW-BVMHFQD";
  };

  otherMachineNames = lib.remove config.networking.hostName (
    builtins.attrNames syncthingMachineIds
  );

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
      group = "users";
      cert = config.deployment.keys.syncthing-cert.path;
      key = config.deployment.keys.syncthing-key.path;
      configDir = "/home/cprussin/.config/syncthing";
      dataDir = "/home/cprussin/.cache/syncthing";
      settings = {
        devices = lib.genAttrs (otherMachineNames ++ ["pegasus"]) (machine: {
          id = syncthingMachineIds."${machine}";
          addresses = ["tcp://${machine}.internal.prussin.net:22000"];
        });
        folders = foldersForCurrentDevice {
          Notes = {
            path = "${config.primary-user.home}/Notes";
            devices = ["crux" "lyra" "pegasus"];
          };
          Projects = {
            path = "${config.primary-user.home}/Projects";
            devices = ["crux" "lyra"];
          };
          Scratch = {
            path = "${config.primary-user.home}/Scratch";
            devices = ["crux" "lyra" "pegasus"];
          };
          Passwords = {
            path = "${config.primary-user.home}/.password-store";
            devices = ["crux" "lyra" "pegasus"];
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
            devices = ["crux" "lyra"];
          };
          Factorio_Saves = {
            path =
              if config.networking.hostName == "crux"
              then "${config.primary-user.home}/Game Saves/Factorio"
              else "${config.primary-user.home}/.factorio/saves";
            devices = ["crux" "lyra"];
          };
        };
        extraOptions.options = {
          listenAddresses = ["tcp://[${network.wireguard6."${config.networking.hostName}".address}]:22000" "tcp://[${network.wireguard4."${config.networking.hostName}".address}]:22000"];
          globalAnnounceEnabled = false;
          localAnnounceEnabled = false;
          natEnabled = false;
          relaysEnabled = false;
          urAccepted = -1;
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
