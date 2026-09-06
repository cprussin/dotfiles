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
  environment = {
    etc."machine-id".text = "89e4f9d000c74a389a33b82baa7c2fb2\n";
    systemPackages = [pkgs.displaylink];
  };
  services = {
    getty.greetingLine = builtins.readFile ./greeting;
    fwupd.enable = true;
    xserver.videoDrivers = ["displaylink" "modesetting"];

    # The audio enhancement filter chain above feeds the raw speaker sink and
    # the two volumes compound, so the raw sink has to sit at 100% for the
    # enhanced sink to reach full loudness.  `/` is tmpfs and we deliberately
    # don't persist WirePlumber's state, so every boot would otherwise start it
    # at WirePlumber's 0.064 default.  `apply-routes.lua` reads this per-device
    # property only when there is no restored volume for the route, so adjusting
    # the volume by hand still works for the rest of the session.
    #
    # This applies to every output route on the card, the 3.5mm jack included,
    # so headphones plugged into the laptop itself also start at full volume.
    pipewire.wireplumber.extraConfig."51-speaker-default-volume" = {
      "monitor.alsa.rules" = [
        {
          matches = [{"device.name" = "alsa_card.pci-0000_c1_00.6";}];
          actions.update-props."device.routes.default-sink-volume" = 1.0;
        }
      ];
    };
  };
  systemd.services = {
    dlm.wantedBy = ["multi-user.target"];

    # DisplayLink outputs on the dock do not survive a sleep/resume cycle:
    # evdi/dlm does not re-establish them on wake, so the monitors stay dark
    # until dlm is restarted (or the dock is replugged) by hand.
    #
    # That restart used to hang off `powerManagement.resumeCommands`, where
    # whether it ran turned on the state of the daemon it was meant to
    # restart.  The option is not a hook of its own: every module's lines are
    # concatenated into the `ExecStop=` of one shared `sleep-actions.service`,
    # and nixpkgs' displaylink module contributes
    # `echo "R" > /tmp/PmMessagesPort_in` to that block ahead of ours.  That
    # path is DisplayLinkManager's FIFO, and opening a FIFO for writing blocks
    # until something opens the read end, so a DisplayLinkManager that came
    # back with that end closed blocked the write, `ExecStop=` was killed at
    # its 90s `TimeoutStopSec`, and the restart behind it never dispatched.
    #
    # A unit of our own sits behind none of that.  It follows
    # systemd.special(7)'s pattern for running something after resume -- pulled
    # in by and ordered before `sleep.target`, with the work in `ExecStop=`,
    # which fires when the resume leaves the unit unneeded -- and its
    # `ExecStart=` is `true`, so nothing can fail and cancel the stop.
    #
    # Resist serialising this against `sleep-actions` to tidy up the overlap.
    # Stop ordering is the reverse of start ordering, so the tempting
    # `before = ["sleep-actions.service"]` would put this unit's `ExecStop=`
    # last -- behind the very write that used to eat the restart.  The two
    # running together cuts both ways: the restart can release an `echo "R"`
    # still waiting for a reader, but it can also close the read end under one
    # that already opened, and the `EPIPE` that follows aborts the rest of that
    # script under the `set -e` NixOS puts in every job script (services run
    # with `IgnoreSIGPIPE=yes` by default, so it arrives as an error rather
    # than a signal).  Today that remainder is dhcpcd's `systemctl reload`, on
    # a window of one 2-byte write.
    dlm-restore = {
      description = "Restore DisplayLink outputs after sleep";
      wantedBy = ["sleep.target"];
      before = ["sleep.target"];
      unitConfig.StopWhenUnneeded = true;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.coreutils}/bin/true";

        # Nothing here needs the restart to have finished, so queue it and let
        # the resume carry on.
        ExecStop = "${pkgs.systemd}/bin/systemctl --no-block restart dlm.service";
      };
    };
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

        # WirePlumber's `node.software-dsp` rule builds the filter chain by
        # matching this string against `node.name` exactly, so a wrong value
        # means the enhanced sink is never created and the option silently does
        # nothing.  nixos-hardware defaults this to the UCM name
        # (`alsa_output.pci-0000_c1_00.6.HiFi__Speaker__sink`), but the speaker
        # card comes up on the generic ALSA card profile here, not under UCM.
        rawDeviceName = "alsa_output.pci-0000_c1_00.6.analog-stereo";
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
        "tank/persisted-state/argo".mountpoint = "/home/${config.primary-user.name}/.config/argo";
        "tank/persisted-state/bluetooth".mountpoint = "/var/lib/bluetooth";
        "tank/persisted-state/chatgpt-desktop".mountpoint = "/home/${config.primary-user.name}/.config/ChatGPT";
        "tank/persisted-state/chromium".mountpoint = "/home/${config.primary-user.name}/.config/chromium";
        "tank/persisted-state/claude-code".mountpoint = "/home/${config.primary-user.name}/.claude";
        "tank/persisted-state/claude-desktop".mountpoint = "/home/${config.primary-user.name}/.config/Claude";
        "tank/persisted-state/codex".mountpoint = "/home/${config.primary-user.name}/.codex";
        "tank/persisted-state/containers".mountpoint = "/home/${config.primary-user.name}/.local/share/containers";
        "tank/persisted-state/direnv-allow".mountpoint = "/home/${config.primary-user.name}/.local/share/direnv/allow";
        "tank/persisted-state/discord".mountpoint = "/home/${config.primary-user.name}/.config/discord";
        "tank/persisted-state/gmail-new-mail-counter".mountpoint = "/home/${config.primary-user.name}/.local/state/gmail-new-mail-counter";
        "tank/persisted-state/iwd".mountpoint = "/var/lib/iwd";
        "tank/persisted-state/log".mountpoint = "/var/log";
        "tank/persisted-state/root-containers".mountpoint = "/var/lib/containers";
        "tank/persisted-state/sunshine".mountpoint = "/home/${config.primary-user.name}/.config/sunshine";
        "tank/persisted-state/syncthing".mountpoint = "/home/${config.primary-user.name}/.cache/syncthing";
        "tank/persisted-state/syncthing-config".mountpoint = "/home/${config.primary-user.name}/.config/syncthing";
        "tank/persisted-state/wluma".mountpoint = "/home/${config.primary-user.name}/.local/share/wluma";
        "tank/persisted-state/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  boot = {
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];
    initrd = {
      availableKernelModules = [];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1" "nvme" "xhci_pci" "thunderbolt" "usb_storage" "uas" "sd_mod" "amdgpu"];
      luksWithKeyDrive."crypt-${disk-id}" = {
        device = "/dev/disk/by-id/${disk-id}";
        key = {
          drive = {inherit (config.fileSystems."/boot") device fsType;};
          file = "luks/${disk-id}/key";
          header = "luks/${disk-id}/header";
          gpgPublicKey = config.flake-inputs.gpg-key;
        };
      };
    };
  };
}
