{ lib, config, ... }:

let
  sources = import ../../../sources.nix;
in

{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces.eth = "enp3s0";

  boot = {
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [];
    preLVMTempMount."/key" = {
      inherit (config.primary-user.secure) device fsType;
    };
    initrd = {
      availableKernelModules = [ "ahci" "ohci_pci" "ehci_pci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
      luks.devices = {
        crypt-ata-SanDisk_SDSSDHII240G_154435401807 = {
          device = "/dev/disk/by-id/ata-SanDisk_SDSSDHII240G_154435401807";
          keyFile = "/key/crypt/orion/ata-SanDisk_SDSSDHII240G_154435401807/key";
          header = "/key/crypt/orion/ata-SanDisk_SDSSDHII240G_154435401807/header";
        };
        crypt-ata-TOSHIBA_DT01ACA200_459YR4PTS = {
          device = "/dev/disk/by-id/ata-TOSHIBA_DT01ACA200_459YR4PTS";
          keyFile = "/key/crypt/orion/ata-TOSHIBA_DT01ACA200_459YR4PTS/key";
          header = "/key/crypt/orion/ata-TOSHIBA_DT01ACA200_459YR4PTS/header";
        };
        crypt-ata-TOSHIBA_DT01ACA200_459YR4STS = {
          device = "/dev/disk/by-id/ata-TOSHIBA_DT01ACA200_459YR4STS";
          keyFile = "/key/crypt/orion/ata-TOSHIBA_DT01ACA200_459YR4STS/key";
          header = "/key/crypt/orion/ata-TOSHIBA_DT01ACA200_459YR4STS/header";
        };
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "tank-fast/system/root";
      fsType = "zfs";
    };

    "/nix" = {
      device = "tank-fast/system/nix";
      fsType = "zfs";
    };

    "/var" = {
      device = "tank-fast/system/var";
      fsType = "zfs";
    };

    "/var/log" = {
      device = "tank-fast/system/var/log";
      fsType = "zfs";
    };

    "/var/log/journal" = {
      device = "tank-fast/system/var/log/journal";
      fsType = "zfs";
    };

    "/home" = {
      device = "tank-fast/home";
      fsType = "zfs";
    };

    "/home/cprussin" = {
      device = "tank-fast/home/cprussin";
      fsType = "zfs";
    };

    "/home/cprussin/Camera" = {
      device = "tank-fast/home/cprussin/Camera";
      fsType = "zfs";
    };

    "/home/cprussin/Mail" = {
      device = "tank-fast/home/cprussin/Mail";
      fsType = "zfs";
    };

    "/home/cprussin/Notes" = {
      device = "tank-fast/home/cprussin/Notes";
      fsType = "zfs";
    };

    "/home/cprussin/Projects" = {
      device = "tank-fast/home/cprussin/Projects";
      fsType = "zfs";
    };

    "/home/cprussin/Scratch" = {
      device = "tank-fast/home/cprussin/Scratch";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/FC73-AD7E";
      fsType = "vfat";
    };
  };

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
}
