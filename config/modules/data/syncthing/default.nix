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
          pegasus.id = "65FQB7W-YNCG6BF-QJIVRNS-NXPMK2F-Z6Q6LN6-KK25P6O-XOG7NNU-FVSQGQ5";
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
    };

    users.users."${config.services.syncthing.user}".extraGroups = [ "keys" ];
  };
}
