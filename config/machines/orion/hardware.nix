{ lib, config, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  wifi-interface = "wlp12s0";

  boot = {
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [];
    crypt-initrd = {
      enable = true;
      device = "/dev/disk/by-id/ata-SanDisk_SDSSDHII240G_154435401807";
      key = {
        device = { inherit (config.primary-user.secure) device fsType; };
        keyPath = "/crypt/orion/root/key";
        headerPath = "/crypt/orion/root/header";
      };
    };
    initrd = {
      availableKernelModules = [ "ahci" "ohci_pci" "ehci_pci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
      luks.devices.crypt-home = {
        device = "/dev/disk/by-id/ata-TOSHIBA_DT01ACA200_459YR4PTS";
        keyFile = "/key/crypt/orion/home/key";
        header = "/key/crypt/orion/home/header";
        preLVM = true;
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/a65c4ce0-a420-4ae0-b1ca-b50516a79e6f";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/CAEC-1652";
      fsType = "vfat";
      options = [ "noauto" ];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/c99fa7ac-d55f-4c4f-baa0-e1a0dda2a45b";
      fsType = "ext4";
    };

    "/secure" = {
      device = "/dev/disk/by-uuid/0063ea5a-2cd5-4202-8e00-862590b49802";
      fsType = "ext4";
    };

    "/var" = {
      device = "/dev/disk/by-uuid/d72292ba-3170-4156-9edd-7a465340f28c";
      fsType = "ext4";
    };
  };

  primary-user.secure = {
    device = "/dev/disk/by-uuid/0063ea5a-2cd5-4202-8e00-862590b49802";
    fsType = "ext4";
    options = [ "noauto" ];
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/d4ac6676-9829-4498-8624-39ced6e5ed9d";
    }
  ];

  nix.maxJobs = lib.mkDefault 8;
}
