{ lib, config, pkgs, ... }:
let
  sources = import ../../../sources.nix;
  zfs = pkgs.callPackage ../../../lib/zfs.nix { };
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
    kernelModules = [ "kvm-intel" "sg" ];
    extraModulePackages = [ ];
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
      fsType = "tmpfs";
      options = [ "defaults" "mode=755" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/770A-9E89";
      fsType = "vfat";
    };
  } // (
    zfs.mkZfsFileSystems {
      "tank/nix".mountpoint = "/nix";
      "tank/data/Camera".mountpoint = "/home/cprussin/Camera";
      "tank/data/Mail".mountpoint = "/home/cprussin/Mail";
      "tank/data/Notes".mountpoint = "/home/cprussin/Notes";
      "tank/data/Projects".mountpoint = "/home/cprussin/Projects";
      "tank/data/Scratch".mountpoint = "/home/cprussin/Scratch";
      "tank/data/Temp".mountpoint = "/home/cprussin/Temp";
      "tank/persisted-state/BitwigStudio".mountpoint = "/home/cprussin/.BitwigStudio";
      "tank/persisted-state/Brave-Browser".mountpoint = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
      "tank/persisted-state/Slack".mountpoint = "/home/cprussin/.config/Slack";
      "tank/persisted-state/bluetooth".mountpoint = "/var/lib/bluetooth";
      "tank/persisted-state/chromium".mountpoint = "/home/cprussin/.config/chromium";
      "tank/persisted-state/direnv-allow".mountpoint = "/home/cprussin/.local/share/direnv/allow";
      "tank/persisted-state/mu".mountpoint = "/home/cprussin/.cache/mu";
      "tank/persisted-state/nixops".mountpoint = "/home/cprussin/.nixops";
      "tank/persisted-state/secrets" = {
        mountpoint = "/secrets";
        neededForBoot = true;
      };
      "tank/persisted-state/syncthing".mountpoint = "/home/cprussin/.config/syncthing";
    }
  );

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
