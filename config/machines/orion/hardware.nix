{ lib, config, pkgs, ... }:
let
  sources = import ../../../sources.nix;
  zfs = pkgs.callPackage ../../../lib/zfs.nix { };
in
{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces.eth = "enp3s0";

  boot = {
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
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
      fsType = "tmpfs";
      options = [ "defaults" "mode=755" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/FC73-AD7E";
      fsType = "vfat";
    };
  } // (
    zfs.mkZfsFileSystems {
      "tank-fast/nix".mountpoint = "/nix";
      "tank-fast/data/Camera".mountpoint = "/home/cprussin/Camera";
      "tank-fast/data/Mail".mountpoint = "/home/cprussin/Mail";
      "tank-fast/data/Notes".mountpoint = "/home/cprussin/Notes";
      "tank-fast/data/Projects".mountpoint = "/home/cprussin/Projects";
      "tank-fast/data/Scratch".mountpoint = "/home/cprussin/Scratch";
      "tank-fast/persisted-state/BitwigStudio".mountpoint = "/home/cprussin/.BitwigStudio";
      "tank-fast/persisted-state/Brave-Browser".mountpoint = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
      "tank-fast/persisted-state/Slack".mountpoint = "/home/cprussin/.config/Slack";
      "tank-fast/persisted-state/direnv-allow".mountpoint = "/home/cprussin/.local/share/direnv/allow";
      "tank-fast/persisted-state/mu".mountpoint = "/home/cprussin/.cache/mu";
      "tank-fast/persisted-state/nixops".mountpoint = "/home/cprussin/.nixops";
      "tank-fast/persisted-state/secrets" = {
        mountpoint = "/secrets";
        neededForBoot = true;
      };
      "tank-fast/persisted-state/syncthing".mountpoint = "/home/cprussin/.config/syncthing";
    }
  );

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 8;
}
