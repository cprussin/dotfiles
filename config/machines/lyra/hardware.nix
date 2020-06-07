{ lib, config, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  wifi-interface = "wlp61s0";

  boot = {
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [];
    crypt-initrd = {
      enable = true;
      device = "/dev/nvme0n1";
      key = {
        device = { inherit (config.primary-user.secure) device fsType; };
        keyPath = "/spitfire";
        headerPath = "/header.img";
      };
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "sd_mod" "sr_mod" ];
      kernelModules = [ "dm-snapshot" "nls_cp437" "nls_iso8859_1" ];
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/c54c75e1-7d3f-461f-a95c-bcfec8ecba9a";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/31DB-5F66";
      fsType = "vfat";
      options = [ "noauto" ];
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

  primary-user.secure = {
    device = "/dev/disk/by-uuid/c00737cd-c285-4aeb-961d-89cb40fbf4bc";
    fsType = "ext4";
    options = [ "noauto" ];
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/ec587887-96e6-43d3-b48b-caa1c678af11";
    }
  ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
