{ lib, config, ... }:

let
  sources = import ../../../sources.nix;

  mkZfsMount = device: mountpoint:
    lib.nameValuePair mountpoint {
      inherit device;
      fsType = "zfs";
    };

  zfsMounts = lib.mapAttrs' mkZfsMount {
    "tank/nix" = "/nix";
    "tank/data/Camera" = "/home/cprussin/Camera";
    "tank/data/Mail" = "/home/cprussin/Mail";
    "tank/data/Notes" = "/home/cprussin/Notes";
    "tank/data/Projects" = "/home/cprussin/Projects";
    "tank/data/Scratch" = "/home/cprussin/Scratch";
    "tank/persisted-state/BitwigStudio" = "/home/cprussin/.BitwigStudio";
    "tank/persisted-state/Brave-Browser" = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
    "tank/persisted-state/Slack" = "/home/cprussin/.config/Slack";
    "tank/persisted-state/bluetooth" = "/var/lib/bluetooth";
    "tank/persisted-state/direnv-allow" = "/home/cprussin/.local/share/direnv/allow";
    "tank/persisted-state/mu" = "/home/cprussin/.mu";
    "tank/persisted-state/nixops" = "/home/cprussin/.nixops";
    "tank/persisted-state/syncthing" = "/home/cprussin/.config/syncthing";
  };
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
      device = "/dev/disk/by-uuid/770A-9E89";
      fsType = "vfat";
    };

    "/secrets" = {
      device = "tank/persisted-state/secrets";
      fsType = "zfs";
      neededForBoot = true;
    };
  };

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
