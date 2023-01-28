{
  lib,
  pkgs,
  config,
  ...
}: let
  sources = import ../../../sources.nix;
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};
  disk-id = "nvme-SAMSUNG_MZVLB512HAJQ-000L7_S3TNNX0K785987";
in {
  imports = [
    "${sources.nixos-hardware}/lenovo/thinkpad/t480s"
    "${sources.nixpkgs}/nixos/modules/installer/scan/not-detected.nix"
  ];

  interfaces = {
    wifi = "wlp61s0";
    eth = "enp0s31f6";
  };

  primary-user.home-manager.wayland.windowManager.sway.config.output.eDP-1.scale = "1.15";

  boot = {
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    preLVMTempMount."/boot" = {
      inherit (config.fileSystems."/boot") device fsType;
      afterMount = ''
        cp /boot/luks/${disk-id}/key \
           /gpg-keys//dev/disk/by-id/${disk-id}/cryptkey.gpg
      '';
    };
    initrd = {
      availableKernelModules = ["xhci_pci" "nvme" "sd_mod" "sr_mod"];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1"];
      luks = {
        gpgSupport = true;
        devices."crypt-${disk-id}" = {
          device = "/dev/disk/by-id/${disk-id}";
          header = "/boot/luks/${disk-id}/header";
          gpgCard = {
            publicKey = ../../modules/security/gpg/pubkey.asc;

            # I don't want this in the store.  However due to how the module is
            # written, I have to put something here, and I have to copy the
            # encrypted key into a specific path, which is done in the
            # preLVMTempMount afterMount command above.
            encryptedPass = pkgs.writeText "dummy" "";
          };
        };
      };
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
        "tank/data/Factorio".mountpoint = "/home/${config.primary-user.name}/.factorio";
        "tank/data/Notes".mountpoint = "/home/${config.primary-user.name}/Notes";
        "tank/data/Passwords".mountpoint = "/home/${config.primary-user.name}/.password-store";
        "tank/data/Projects".mountpoint = "/home/${config.primary-user.name}/Projects";
        "tank/data/Scratch".mountpoint = "/home/${config.primary-user.name}/Scratch";
        "tank/data/Stardew Valley Saves".mountpoint = "/home/${config.primary-user.name}/.config/StardewValley/Saves";
        "tank/persisted-state/BitwigStudio".mountpoint = "/home/${config.primary-user.name}/.BitwigStudio";
        "tank/persisted-state/Brave-Browser".mountpoint = "/home/${config.primary-user.name}/.config/BraveSoftware/Brave-Browser";
        "tank/persisted-state/Element".mountpoint = "/home/${config.primary-user.name}/.config/Element";
        "tank/persisted-state/Slack".mountpoint = "/home/${config.primary-user.name}/.config/Slack";
        "tank/persisted-state/Steam".mountpoint = "/home/${config.primary-user.name}/.local/share/Steam";
        "tank/persisted-state/Zulip".mountpoint = "/home/${config.primary-user.name}/.config/Zulip";
        "tank/persisted-state/alsa".mountpoint = "/var/lib/alsa";
        "tank/persisted-state/bluetooth".mountpoint = "/var/lib/bluetooth";
        "tank/persisted-state/chromium".mountpoint = "/home/${config.primary-user.name}/.config/chromium";
        "tank/persisted-state/direnv-allow".mountpoint = "/home/${config.primary-user.name}/.local/share/direnv/allow";
        "tank/persisted-state/discord".mountpoint = "/home/${config.primary-user.name}/.config/discord";
        "tank/persisted-state/log".mountpoint = "/var/log";
        "tank/persisted-state/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  swapDevices = [];

  nix.settings.max-jobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
}
