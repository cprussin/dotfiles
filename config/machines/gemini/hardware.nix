{
  lib,
  pkgs,
  ...
}: let
  sources = import ../../../sources.nix;
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};
in {
  imports = [
    "${sources.nixos-hardware}/lenovo/thinkpad/t480s"
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces = {
    wifi = "wlp61s0";
    eth = "enp0s31f6";
  };

  primary-user = {
    secure.luksDrives = ["nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987"];
    home-manager.wayland.windowManager.sway.config.output.eDP-1.scale = "1.15";
  };

  boot = {
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    initrd = {
      availableKernelModules = ["xhci_pci" "nvme" "sd_mod" "sr_mod"];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1"];
    };
  };

  fileSystems =
    {
      "/" = {
        fsType = "tmpfs";
        options = ["defaults" "mode=755"];
      };

      "/boot" = {
        device = "/dev/disk/by-uuid/770A-9E89";
        fsType = "vfat";
      };
    }
    // (
      zfs.mkZfsFileSystems {
        "tank/nix".mountpoint = "/nix";
        "tank/data/Notes".mountpoint = "/home/cprussin/Notes";
        "tank/data/Projects".mountpoint = "/home/cprussin/Projects";
        "tank/data/Scratch".mountpoint = "/home/cprussin/Scratch";
        "tank/persisted-state/BitwigStudio".mountpoint = "/home/cprussin/.BitwigStudio";
        "tank/persisted-state/Brave-Browser".mountpoint = "/home/cprussin/.config/BraveSoftware/Brave-Browser";
        "tank/persisted-state/Element".mountpoint = "/home/cprussin/.config/Element";
        "tank/persisted-state/Slack".mountpoint = "/home/cprussin/.config/Slack";
        "tank/persisted-state/Steam".mountpoint = "/home/cprussin/.local/share/Steam";
        "tank/persisted-state/Zulip".mountpoint = "/home/cprussin/.config/Zulip";
        "tank/persisted-state/alsa".mountpoint = "/var/lib/alsa";
        "tank/persisted-state/bluetooth".mountpoint = "/var/lib/bluetooth";
        "tank/persisted-state/chromium".mountpoint = "/home/cprussin/.config/chromium";
        "tank/persisted-state/direnv-allow".mountpoint = "/home/cprussin/.local/share/direnv/allow";
        "tank/persisted-state/discord".mountpoint = "/home/cprussin/.config/discord";
        "tank/persisted-state/factorio".mountpoint = "/home/cprussin/.factorio";
        "tank/persisted-state/log".mountpoint = "/var/log";
        "tank/persisted-state/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  swapDevices = [];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
}
