{
  config,
  lib,
  pkgs,
  ...
}: let
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};
  disk-id = "nvme-WD_BLACK_SN850X_8000GB_24456N802672";
in {
  imports = [
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "lyra";
    hostId = "73898c5c";
  };
  environment.etc."machine-id".text = "89e4f9d000c74a389a33b82baa7c2fb2\n";
  services = {
    getty.greetingLine = builtins.readFile ./greeting;
    fwupd.enable = true;
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    framework = {
      enableKmod = true;
      laptop13.audioEnhancement = {
        enable = true;
        hideRawDevice = false;
      };
    };
  };

  swapDevices = [];
  fileSystems =
    {
      "/" = {
        fsType = "tmpfs";
        options = ["defaults" "mode=755"];
      };

      "/boot" = {
        device = "/dev/disk/by-uuid/69FD-AA98";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };
    }
    // (
      zfs.mkZfsFileSystems {
        "tank/data/Factorio".mountpoint = "/home/${config.primary-user.name}/.factorio";
        "tank/data/Notes".mountpoint = "/home/${config.primary-user.name}/Notes";
        "tank/data/Passwords".mountpoint = "/home/${config.primary-user.name}/.password-store";
        "tank/data/Projects".mountpoint = "/home/${config.primary-user.name}/Projects";
        "tank/data/Scratch".mountpoint = "/home/${config.primary-user.name}/Scratch";
        "tank/data/Stardew Valley Saves".mountpoint = "/home/${config.primary-user.name}/.config/StardewValley/Saves";
        "tank/nix".mountpoint = "/nix";
        "tank/persisted-state/BitwigStudio".mountpoint = "/home/${config.primary-user.name}/.BitwigStudio";
        "tank/persisted-state/Brave-Browser".mountpoint = "/home/${config.primary-user.name}/.config/BraveSoftware/Brave-Browser";
        "tank/persisted-state/Element".mountpoint = "/home/${config.primary-user.name}/.config/Element";
        "tank/persisted-state/PrismLauncher".mountpoint = "/home/${config.primary-user.name}/.share/PrismLauncher";
        "tank/persisted-state/PrusaSlicer".mountpoint = "/home/${config.primary-user.name}/.config/PrusaSlicer";
        "tank/persisted-state/Slack".mountpoint = "/home/${config.primary-user.name}/.config/Slack";
        "tank/persisted-state/Steam".mountpoint = "/home/${config.primary-user.name}/.local/share/Steam";
        "tank/persisted-state/TelegramDesktop".mountpoint = "/home/${config.primary-user.name}/.local/share/TelegramDesktop";
        "tank/persisted-state/Zulip".mountpoint = "/home/${config.primary-user.name}/.config/Zulip";
        "tank/persisted-state/alsa".mountpoint = "/var/lib/alsa";
        "tank/persisted-state/bluetooth".mountpoint = "/var/lib/bluetooth";
        "tank/persisted-state/chromium".mountpoint = "/home/${config.primary-user.name}/.config/chromium";
        "tank/persisted-state/containers".mountpoint = "/home/${config.primary-user.name}/.local/share/containers";
        "tank/persisted-state/direnv-allow".mountpoint = "/home/${config.primary-user.name}/.local/share/direnv/allow";
        "tank/persisted-state/discord".mountpoint = "/home/${config.primary-user.name}/.config/discord";
        "tank/persisted-state/gmail-new-mail-counter".mountpoint = "/home/${config.primary-user.name}/.local/state/gmail-new-mail-counter";
        "tank/persisted-state/iwd".mountpoint = "/var/lib/iwd";
        "tank/persisted-state/log".mountpoint = "/var/log";
        "tank/persisted-state/syncthing".mountpoint = "/home/${config.primary-user.name}/.cache/syncthing";
        "tank/persisted-state/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  boot = {
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
    preLVMTempMount."/boot" = {
      inherit (config.fileSystems."/boot") device fsType;
      # TODO Could I, or should I, use colmena keys here?
      afterMount = ''
        cp /boot/luks/${disk-id}/key /gpg-keys/dev/disk/by-id/${disk-id}/cryptkey.gpg
      '';
    };
    initrd = {
      availableKernelModules = [];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1" "nvme" "xhci_pci" "thunderbolt" "usb_storage" "uas" "sd_mod" "amdgpu"];
      luks = {
        gpgSupport = true;
        devices."crypt-${disk-id}" = {
          device = "/dev/disk/by-id/${disk-id}";
          header = "/boot/luks/${disk-id}/header";
          gpgCard = {
            publicKey = config.flake-inputs.gpg-key;
            gracePeriod = 99999;

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
}
