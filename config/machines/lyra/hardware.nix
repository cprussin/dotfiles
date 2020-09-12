{ lib, config, ... }:

let
  sources = import ../../../sources.nix;
in

{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces = {
    wifi = "wlp61s0";
    eth = "enp0s31f6";
  };

  boot = {
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [];
    preLVMTempMount."/key" = {
      inherit (config.primary-user.secure) device fsType;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
      luks.devices.crypt-nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987 = {
        device = "/dev/disk/by-id/nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987";
        keyFile = "/key/crypt/lyra/nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987/key";
        header = "/key/crypt/lyra/nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987/header";
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "tank/system/root";
      fsType = "zfs";
    };

    "/nix" = {
      device = "tank/system/nix";
      fsType = "zfs";
    };

    "/var" = {
      device = "tank/system/var";
      fsType = "zfs";
    };

    "/var/log" = {
      device = "tank/system/var/log";
      fsType = "zfs";
    };

    "/var/log/journal" = {
      device = "tank/system/var/log/journal";
      fsType = "zfs";
    };

    "/home" = {
      device = "tank/home";
      fsType = "zfs";
    };

    "/home/cprussin" = {
      device = "tank/home/cprussin";
      fsType = "zfs";
    };

    "/home/cprussin/Camera" = {
      device = "tank/home/cprussin/Camera";
      fsType = "zfs";
    };

    "/home/cprussin/Mail" = {
      device = "tank/home/cprussin/Mail";
      fsType = "zfs";
    };

    "/home/cprussin/Notes" = {
      device = "tank/home/cprussin/Notes";
      fsType = "zfs";
    };

    "/home/cprussin/Projects" = {
      device = "tank/home/cprussin/Projects";
      fsType = "zfs";
    };

    "/home/cprussin/Scratch" = {
      device = "tank/home/cprussin/Scratch";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/770A-9E89";
      fsType = "vfat";
    };
  };

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
