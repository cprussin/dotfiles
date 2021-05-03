{ lib, pkgs, ... }:
let
  sources = import ../../../sources.nix;
  zfs = pkgs.callPackage ../../../lib/zfs.nix { };
in
{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces = {
    eth = "enp3s0";
    wifi = "wlp12s0";
  };

  primary-user.secure.luksDrives = [
    "ata-SanDisk_SDSSDHII240G_154435401807"
    "ata-TOSHIBA_DT01ACA200_459YR4PTS"
    "ata-TOSHIBA_DT01ACA200_459YR4STS"
  ];

  boot = {
    kernelModules = [ "kvm-amd" "sg" ];
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [ "ahci" "ohci_pci" "ehci_pci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
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
      "tank-big/Temp".mountpoint = "/home/cprussin/Temp";
      "tank-fast/nix".mountpoint = "/nix";
      "tank-fast/data/Camera".mountpoint = "/home/cprussin/Camera";
      "tank-fast/data/Notes".mountpoint = "/home/cprussin/Notes";
      "tank-fast/data/Projects".mountpoint = "/home/cprussin/Projects";
      "tank-fast/data/Scratch".mountpoint = "/home/cprussin/Scratch";
      "tank-fast/persisted-state/BitwigStudio".mountpoint = "/home/cprussin/.BitwigStudio";
      "tank-fast/persisted-state/Brave-Browser".mountpoint = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
      "tank-fast/persisted-state/Slack".mountpoint = "/home/cprussin/.config/Slack";
      "tank-fast/persisted-state/alsa".mountpoint = "/var/lib/alsa";
      "tank-fast/persisted-state/chromium".mountpoint = "/home/cprussin/.config/chromium";
      "tank-fast/persisted-state/direnv-allow".mountpoint = "/home/cprussin/.local/share/direnv/allow";
      "tank-fast/persisted-state/nixops".mountpoint = "/home/cprussin/.nixops";
      "tank-fast/persisted-state/secrets" = {
        mountpoint = "/secrets";
        neededForBoot = true;
      };
    }
  );

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 8;
}
