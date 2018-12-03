{ lib, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ata_piix" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ccb9682c-891a-49d2-932f-a50b47a14740";
      fsType = "ext4";
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-uuid/8c7c6abc-5eed-4ebd-9971-5d85e325e236";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/15DE-41D5";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/a33b694c-1cc1-4ce3-ba6d-43728648e289";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/b7bd2a7c-c549-4d46-99ec-a9157d8fc1c9"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
