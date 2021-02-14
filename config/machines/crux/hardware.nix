{ lib, config, pkgs, ... }:
let
  sources = import ../../../sources.nix;

  passwords = pkgs.callPackage ../../../lib/passwords.nix { };

  zfs = pkgs.callPackage ../../../lib/zfs.nix { };

  getLuksFile = drive: file:
    passwords.getBase64EncodedPassword "Infrastructure/luks/crux/${drive}/${file}";

  zfsDrives = [
    "ata-ST10000VN0008-2JJ101_ZHZ06Y2A"
    "ata-ST10000VN0008-2JJ101_ZHZ08V0G"
    "ata-ST10000VN0008-2JJ101_ZHZ0L7WG"
  ];

  drives = zfsDrives ++ [
    "usb-WD_Elements_25A3_564347414D34534D-0:0"
  ];
in
{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  detachedLuksWithNixopsKeys = builtins.listToAttrs (
    map
      (
        drive: lib.nameValuePair drive {
          key = getLuksFile drive "key";
          header = getLuksFile drive "header";
        }
      )
      drives
  );

  systemd.services.import-tank = {
    after = map (drive: "unlock-${drive}.service") zfsDrives;
    wants = map (drive: "unlock-${drive}.service") zfsDrives;
    wantedBy = [ "zfs.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.zfs}/bin/zpool import tank";
      RemainAfterExit = true;
      Type = "oneshot";
    };
  };

  boot = {
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    preLVMTempMount."/key" = {
      inherit (config.fileSystems."/boot") device fsType;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
      luks.devices.crypt-nvme-WDS500G3X0C-00SJG0_2017A3806951 = {
        device = "/dev/disk/by-id/nvme-WDS500G3X0C-00SJG0_2017A3806951";
        keyFile = "/key/crypt/nvme-WDS500G3X0C-00SJG0_2017A3806951/key";
        header = "/key/crypt/nvme-WDS500G3X0C-00SJG0_2017A3806951/header";
      };
    };
  };

  fileSystems = {
    "/" = {
      fsType = "tmpfs";
      options = [ "defaults" "mode=755" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/4641-BCB3";
      fsType = "vfat";
    };
  } // (
    zfs.mkZfsFileSystems {
      "tank-fast/nix".mountpoint = "/nix";
      "tank-fast/persisted-state/acme".mountpoint = "/var/lib/acme";
      "tank-fast/persisted-state/borg-cache".mountpoint = "/root/.cache/borg";
      "tank-fast/persisted-state/hass".mountpoint = "/var/lib/hass";
      "tank-fast/persisted-state/plex".mountpoint = "/var/lib/plex";
      "tank-fast/persisted-state/secrets" = {
        mountpoint = "/secrets";
        neededForBoot = true;
      };
      "tank-fast/persisted-state/syncthing".mountpoint = "/home/cprussin/.config/syncthing";
    }
  );

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 16;
}
