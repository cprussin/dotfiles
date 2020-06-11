{ lib, config, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  wifi-interface = "wlp12s0";

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
        crypt-root = {
          device = "/dev/disk/by-id/ata-SanDisk_SDSSDHII240G_154435401807";
          keyFile = "/key/crypt/orion/root/key";
          header = "/key/crypt/orion/root/header";
        };
        crypt-home = {
          device = "/dev/disk/by-id/ata-TOSHIBA_DT01ACA200_459YR4PTS";
          keyFile = "/key/crypt/orion/home/key";
          header = "/key/crypt/orion/home/header";
        };
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/a65c4ce0-a420-4ae0-b1ca-b50516a79e6f";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/98BB-45B7";
      fsType = "vfat";
    };

    "/home" = {
      device = "/dev/disk/by-uuid/c99fa7ac-d55f-4c4f-baa0-e1a0dda2a45b";
      fsType = "ext4";
    };

    "/var" = {
      device = "/dev/disk/by-uuid/d72292ba-3170-4156-9edd-7a465340f28c";
      fsType = "ext4";
    };
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/d4ac6676-9829-4498-8624-39ced6e5ed9d";
    }
  ];

  nix.maxJobs = lib.mkDefault 8;
}
