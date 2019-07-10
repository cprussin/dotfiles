{ lib, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot = {
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [];
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "uas" "usbcore" "usb_storage" "vfat" "nls_cp437" "nls_iso8859_1" "loop" ];
      preLVMCommands = lib.mkMerge [
        (lib.mkBefore ''
          mkdir -m 0755 -p /key
          sleep 2
          mount -n -t vfat -o ro "/dev/disk/by-uuid/642C-BBD5" /key
        '')
        (lib.mkAfter ''
          umount /key
          rmdir /key
        '')
      ];
      luks.devices.crypta = {
        device = "/dev/nvme0n1";
        keyFile = "/key/spitfire";
        header = "/key/header.img";
        preLVM = true;
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/c54c75e1-7d3f-461f-a95c-bcfec8ecba9a";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/642C-BBD5";
      fsType = "vfat";
    };

    "/home" = {
      device = "/dev/disk/by-uuid/1e8a4c25-733b-4571-9586-fd210c858581";
      fsType = "ext4";
    };

    "/var" = {
      device = "/dev/disk/by-uuid/faf29f4a-44f9-4703-8c07-2f257aa51fa2";
      fsType = "ext4";
    };
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/ec587887-96e6-43d3-b48b-caa1c678af11";
    }
  ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
