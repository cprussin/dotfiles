{
  config,
  lib,
  pkgs,
  ...
}: let
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};
  disk-id = "nvme-WD_BLACK_SN850X_8000GB_24456N802672";

  # DisplayLinkManager takes power-management commands on one FIFO and
  # acknowledges them on the other.  Opening a FIFO blocks until the far end is
  # opened, and `read -t` bounds the read but not the open, so every touch of
  # these below is wrapped in `timeout`, which can interrupt a blocked open
  # where `read -t` cannot.  That matters because `sleep-actions.service` is
  # `Type=oneshot`, which disables `TimeoutStartSec`: a block on the way down
  # hangs the suspend outright rather than timing out.
  pmIn = "/tmp/PmMessagesPort_in";
  pmOut = "/tmp/PmMessagesPort_out";

  # Set when DisplayLinkManager did not acknowledge the suspend.  `/run` is
  # tmpfs, so this cannot survive a boot and be believed on a later resume.
  unacknowledged = "/run/displaylink-suspend-unacknowledged";

  # nixpkgs' pre-sleep block has two bugs and both bite here, so replace it
  # rather than appending to it -- displaylink is the only contributor to
  # `powerDownCommands` on this machine, so `mkForce` below loses nothing.
  #
  #   - It drains the acknowledgement FIFO with an unbounded
  #     `while read -n 1 -t 1 _ < ${pmOut}`.  With a stale port and no
  #     DisplayLinkManager to open the write end, that hangs the suspend
  #     indefinitely -- nixpkgs#281104.
  #   - It then guards upstream's acknowledgement read with `[ -f ${pmOut} ]`,
  #     and `-f` is false for a FIFO, so the wait never runs and the machine
  #     suspends while DisplayLinkManager is still tearing its USB side down.
  #     Upstream's own `suspend_dlm` waits unconditionally.
  #
  # The second is the interesting one: it is the most likely reason
  # DisplayLinkManager was coming back unable to drive the outputs at all.
  # That is a hypothesis, not something observed.
  #
  # Worst case this costs 2 + 2 + 10s, paid when the ports exist but nothing
  # answers on them.  Nothing removes them once DisplayLinkManager has created
  # them and `/tmp` lives for the boot, so a stopped daemon does not make this
  # fast -- only an absent one does.  That bound also applies on the way to
  # shutdown, where `powerDownCommands` runs again via `post-boot.service`'s
  # `preStop`.  Against the unbounded version that is a win where it hung --
  # until `post-boot` hit its 90s `TimeoutStopSec` -- and a loss where the
  # daemon is up but silent, which cost nothing before only because the `-f`
  # guard skipped the wait entirely.
  suspend-dlm = pkgs.writeShellScript "suspend-dlm" ''
    ${pkgs.coreutils}/bin/timeout 2 ${pkgs.bash}/bin/bash -c \
      'while read -n 1 -t 1 _ < ${pmOut}; do : ; done' || true

    ${pkgs.coreutils}/bin/timeout 2 ${pkgs.bash}/bin/bash -c \
      'echo "S" > ${pmIn}' || true

    # Whether the acknowledgement arrives is the one signal available for
    # whether DisplayLinkManager suspended cleanly, so hand it to the resume
    # side rather than guessing there.
    if ${pkgs.coreutils}/bin/timeout 10 ${pkgs.bash}/bin/bash -c \
      'read -n 1 _ < ${pmOut}'
    then
      ${pkgs.coreutils}/bin/rm -f ${unacknowledged}
    else
      ${pkgs.coreutils}/bin/touch ${unacknowledged}
    fi
  '';

  # The bet: a DisplayLinkManager that acknowledged the suspend should resume
  # on the "R" nixpkgs sends and keep its evdi devices, leaving the outputs
  # where they are and sway and kanshi with nothing to rebuild.  Restarting dlm
  # gets the monitors back too, but only by dropping every DisplayLink output
  # first, so spend it only where the handshake says it is needed.  If the
  # outputs come back dark anyway, this is the assumption that was wrong, and
  # making the restart unconditional again is the fix.
  #
  # Deliberately not a liveness check on ${pmIn}: `dlm.service` is
  # `Restart=always` with `RestartSec=5`, so something holds its read end
  # again within seconds of any crash, and such a check would answer "fine"
  # for the case this exists to catch.
  restore-dlm = pkgs.writeShellScript "restore-dlm" ''
    if [ -e ${unacknowledged} ]
    then
      ${pkgs.systemd}/bin/systemctl --no-block restart dlm.service
      restart=$?

      # Consume it.  Every suspend rewrites this, but leaving a stale one set
      # would make any later stop of this unit drop the outputs for nothing.
      ${pkgs.coreutils}/bin/rm -f ${unacknowledged}

      # `writeShellScript` adds no `set -e`, so without this a failed restart
      # would exit 0 through the `rm` and leave no trace -- on the one path
      # where the monitors depend on it having worked.
      exit "$restart"
    fi
  '';
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

    # Runs `restore-dlm` above on resume, which restarts dlm only when the
    # suspend handshake went unacknowledged.
    #
    # That restart used to hang off `powerManagement.resumeCommands`, where
    # whether it ran turned on the state of the daemon it was meant to restart.
    # The option is not a hook of its own: every module's lines are
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
    # last -- behind the very write that used to eat the restart.
    #
    # When the restart does fire and dlm is dead rather than merely silent,
    # that write is itself blocked in `open()`, and it stays blocked until
    # `sleep-actions` hits its 90s `TimeoutStopSec` unless the new daemon
    # reuses the same FIFO -- taking the rest of that script, dhcpcd's
    # `systemctl reload`, with it.  That is the cost of not waiting, and it is
    # only paid on resumes that were already going wrong.
    dlm-restore = {
      description = "Restore DisplayLink outputs after sleep";
      wantedBy = ["sleep.target"];
      before = ["sleep.target"];
      unitConfig.StopWhenUnneeded = true;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.coreutils}/bin/true";
        ExecStop = restore-dlm;
      };
    };
  };

  # Replaces nixpkgs' block outright; see `suspend-dlm` above for why, and for
  # why nothing is lost by forcing it.  The matching `resumeCommands` is left
  # alone: dhcpcd contributes to that one too.
  powerManagement.powerDownCommands = lib.mkForce "${suspend-dlm}";

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
