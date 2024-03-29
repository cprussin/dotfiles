{
  lib,
  config,
  pkgs,
  modulesPath,
  ...
}: let
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};

  zfsDrives = [
    "ata-ST10000VN0008-2JJ101_ZHZ06Y2A"
    "ata-ST10000VN0008-2JJ101_ZHZ08V0G"
    "ata-ST10000VN0008-2JJ101_ZHZ0L7WG"
  ];
in {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../../modules/system/devices/cdrom
  ];

  interfaces.eth = "enp8s0";

  detachedLuksWithNixopsKeys =
    {
      "${config.backupDisk.diskId}" = {
        inherit (config.backupDisk) filenameBase;
      };
    }
    // builtins.listToAttrs (map (drive: lib.nameValuePair drive {}) zfsDrives);

  systemd.services.import-tank = {
    after = map (drive: "unlock-${drive}.service") zfsDrives;
    bindsTo = map (drive: "unlock-${drive}.service") zfsDrives;
    script = ''
      if [ "$(${pkgs.zfs}/bin/zpool get -H health tank | cut -f 3)" = "ONLINE" ]; then
        echo "Already imported: tank"
      else
        ${pkgs.zfs}/bin/zpool import tank
      fi
      ${pkgs.zfs}/bin/zfs mount -a
    '';
    preStop = "${pkgs.zfs}/bin/zpool export tank";
    serviceConfig = {
      RemainAfterExit = true;
      Type = "oneshot";
    };
  };

  boot = {
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    preLVMTempMount."/key" = {
      inherit (config.fileSystems."/boot") device fsType;
    };
    initrd = {
      availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" "e1000e" "igb"];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1"];
      luks.devices.crypt-nvme-WDS500G3X0C-00SJG0_2017A3806951 = {
        device = "/dev/disk/by-id/nvme-WDS500G3X0C-00SJG0_2017A3806951";
        keyFile = "/key/crypt/nvme-WDS500G3X0C-00SJG0_2017A3806951/key";
        header = "/key/crypt/nvme-WDS500G3X0C-00SJG0_2017A3806951/header";
      };
    };
  };

  fileSystems =
    {
      "/" = {
        fsType = "tmpfs";
        options = ["defaults" "mode=755"];
      };

      "/boot" = {
        device = "/dev/disk/by-uuid/4641-BCB3";
        fsType = "vfat";
      };
    }
    // (
      zfs.mkZfsFileSystems {
        "tank-fast/nix".mountpoint = "/nix";
        "tank-fast/log".mountpoint = "/var/log";
        "tank-fast/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  swapDevices = [];

  nix.settings.max-jobs = lib.mkDefault 16;
}
