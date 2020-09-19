{ lib, config, ... }:

let
  sources = import ../../../sources.nix;

  mkZfsMount = device: mountpoint:
    lib.nameValuePair mountpoint {
      inherit device;
      fsType = "zfs";
    };

  zfsMounts = lib.mapAttrs' mkZfsMount {
    "tank-fast/nix" = "/nix";
    "tank-fast/data/Camera" = "/home/cprussin/Camera";
    "tank-fast/data/Mail" = "/home/cprussin/Mail";
    "tank-fast/data/Notes" = "/home/cprussin/Notes";
    "tank-fast/data/Projects" = "/home/cprussin/Projects";
    "tank-fast/data/Scratch" = "/home/cprussin/Scratch";
    "tank-fast/persisted-state/BitwigStudio" = "/home/cprussin/.BitwigStudio";
    "tank-fast/persisted-state/Brave-Browser" = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
    "tank-fast/persisted-state/Slack" = "/home/cprussin/.config/Slack";
    "tank-fast/persisted-state/direnv-allow" = "/home/cprussin/.local/share/direnv/allow";
    "tank-fast/persisted-state/mu" = "/home/cprussin/.mu";
    "tank-fast/persisted-state/nixops" = "/home/cprussin/.nixops";
    "tank-fast/persisted-state/syncthing" = "/home/cprussin/.config/syncthing";
  };
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
    postBootCommands = ''
      install -m 0700 -g users -o cprussin -d /home/cprussin/.config
      install -m 0700 -g users -o cprussin -d /home/cprussin/.config/BraveSoftware
      install -m 0700 -g users -o cprussin -d /home/cprussin/.local
      install -m 0700 -g users -o cprussin -d /home/cprussin/.local/share
      install -m 0700 -g users -o cprussin -d /home/cprussin/.local/share/direnv
    '';
  };

  fileSystems = zfsMounts // {
    "/" = {
      fsType = "tmpfs";
      options = [ "defaults" "mode=755" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/FC73-AD7E";
      fsType = "vfat";
    };

    "/secrets" = {
      device = "tank-fast/persisted-state/secrets";
      fsType = "zfs";
      neededForBoot = true;
    };
  };

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
}
