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
      luks.devices.crypt-ata-SanDisk_SDSSDHII240G_154435401807 = {
        device = "/dev/disk/by-id/ata-SanDisk_SDSSDHII240G_154435401807";
        keyFile = "/key/crypt/orion/ata-SanDisk_SDSSDHII240G_154435401807/key";
        header = "/key/crypt/orion/ata-SanDisk_SDSSDHII240G_154435401807/header";
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

    "/boot" = {
      device = "/dev/disk/by-uuid/FC73-AD7E";
      fsType = "vfat";
    };
  };

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
}
