{ lib, config, pkgs, ... }:

let
  sources = import ../../../sources.nix;

  passwords = pkgs.callPackage ../../../lib/passwords.nix {};

  getLuksFile = drive: file:
    passwords.get-base64-encoded-password "Infrastructure/luks/crux/${drive}/${file}";

  drives = [
    "ata-ST10000VN0008-2JJ101_ZHZ06Y2A"
    "ata-ST10000VN0008-2JJ101_ZHZ08V0G"
    "ata-ST10000VN0008-2JJ101_ZHZ0L7WG"
  ];
in

{
  imports = [
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  detachedLuksWithNixopsKeys = builtins.listToAttrs (
    map (
      drive: lib.nameValuePair drive {
        key = getLuksFile drive "key";
        header = getLuksFile drive "header";
      }
    ) drives
  );

  systemd.services.import-zfs = {
    enable = true;
    after = map (drive: "unlock-${drive}.service") drives;
    wants = map (drive: "unlock-${drive}.service") drives;
    wantedBy = [ "zfs.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.zfs}/bin/zpool import -a -d /dev/mapper";
      Type = "oneshot";
    };
  };

  boot = {
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [];
    preLVMTempMount."/key" = {
      inherit (config.fileSystems."/boot") device fsType;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
      luks.devices.crypt-root = {
        device = "/dev/disk/by-id/nvme-WDS500G3X0C-00SJG0_2017A3806951";
        keyFile = "/key/key";
        header = "/key/header";
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/95c8e3e0-15f9-42e7-82c2-d222126d6038";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/B68B-4DDB";
      fsType = "vfat";
    };

    "/home" = {
      device = "/dev/disk/by-uuid/14611807-00b1-419d-b858-8e2eccc4779a";
      fsType = "ext4";
    };

    "/var" = {
      device = "/dev/disk/by-uuid/a124c568-a0ff-441a-a562-e9901acdf6f3";
      fsType = "ext4";
    };
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/233f6e4b-0c92-44c6-a903-b9079d448b1e";
    }
  ];

  nix.maxJobs = lib.mkDefault 16;
}
