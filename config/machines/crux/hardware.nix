{
  lib,
  config,
  pkgs,
  modulesPath,
  ...
}: let
  zfs = pkgs.callPackage ../../../lib/zfs.nix {};

  # The userland the rest of the ZFS module is wired to, and so the one matched
  # to the kernel module actually loaded.  `pkgs.zfs` is the same derivation
  # only for as long as nobody sets this option.
  zfsPackage = config.boot.zfs.package;

  root-disk-id = "nvme-WDS500G3X0C-00SJG0_2017A3806951";

  zfsDrives = [
    "ata-ST10000VN0008-2JJ101_ZHZ06Y2A"
    "ata-ST10000VN0008-2JJ101_ZHZ08V0G"
    "ata-ST10000VN0008-2JJ101_ZHZ0L7WG"
  ];
in {
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ../../modules/system/devices/cdrom
  ];

  interfaces.eth = "enp8s0";

  detachedLuksWithNixopsKeys =
    {
      "${config.backupDisk.diskId}" = {
        inherit (config.backupDisk) filenameBase;
      };
    }
    // builtins.listToAttrs (map (drive: lib.nameValuePair drive {}) zfsDrives);

  systemd.services.import-tank = {
    after = map (drive: "unlock-${drive}.service") zfsDrives;
    bindsTo = map (drive: "unlock-${drive}.service") zfsDrives;
    script = ''
      # pipefail so that a failing `zfs list` below cannot be masked by the
      # `sort` that reads its output.  The `zpool get` pipeline is an `if`
      # condition, which `set -e` exempts either way.
      set -o pipefail

      # `LC_ALL=C` because the state name is translated, and under a locale
      # that renders it as anything else this runs `zpool import` against an
      # already-imported pool and fails the unit.
      if [ "$(LC_ALL=C ${zfsPackage}/bin/zpool get -H health tank | cut -f 3)" = "ONLINE" ]; then
        echo "Already imported: tank"
      else
        ${zfsPackage}/bin/zpool import tank
      fi

      # Deliberately not `zfs mount -a`, which mounts every mountable dataset
      # on every imported pool, not just the one this unit imports.
      # `run-backup` holds tank-backup imported while it replicates, and the
      # datasets it receives are copies of tank's -- mountpoints included, so
      # /home/cprussin and friends resolve to the same paths on both pools.  A
      # deploy restarts this unit, `zfs mount -a` then mounted the external
      # disk over the internal one, and every write after that silently landed
      # on the backup drive.  `run-backup` now imports under an altroot so
      # those mountpoints cannot name an internal path at all; this loop is
      # the other half, so nothing here can mount a pool it does not own.
      #
      # The filter covers what applies to this pool: skip canmount=off and
      # canmount=noauto, skip what is already mounted, and skip legacy and
      # mountpoint=none datasets, neither of which starts with a `/`.
      #
      # `zfs mount -a` additionally skips, silently, three states a per-dataset
      # `zfs mount` treats as an error: zoned datasets, encrypted ones whose
      # key is unavailable, and ones holding a receive-resume token.  None can
      # arise from anything in this repo -- tank is LUKS rather than ZFS-native
      # encryption, there are no zones, and nothing here receives into tank --
      # so the divergence is deliberate: a hand-run `zfs recv -s` into tank
      # that got interrupted would leave a resume token, and this unit should
      # say so rather than skip past it.
      #
      # Sorted on the mountpoint rather than the dataset name because a dataset
      # whose mountpoint is a parent path has to be mounted before anything
      # nested under it, or it hides what is already there, and the two orders
      # diverge wherever a mountpoint is not a mirror of the name it hangs off.
      # `LC_ALL=C` for byte order, which is all that invariant needs; ZFS's own
      # comparator differs in ways that do not bear on it.
      datasets=$(${zfsPackage}/bin/zfs list -Hro name,canmount,mounted,mountpoint -t filesystem tank | LC_ALL=C sort -t "$(printf '\t')" -k 4)

      # Failures are collected rather than fatal, again as `zfs mount -a` does.
      # `/` on this machine is a tmpfs, so one dataset that cannot mount must
      # not abort the loop and leave every later one unmounted -- writes to
      # those paths would go to RAM.  A herestring rather than a pipe so the
      # loop is not a subshell and `failed` survives it.
      failed=0
      while IFS=$'\t' read -r name canmount mounted mountpoint
      do
        [ "$canmount" = on ] && [ "$mounted" = no ] || continue
        case "$mountpoint" in
          /*) ${zfsPackage}/bin/zfs mount "$name" || failed=1 ;;
        esac
      done <<< "$datasets"
      [ "$failed" -eq 0 ]
    '';
    serviceConfig = {
      RemainAfterExit = true;
      Type = "oneshot";
    };
  };

  # The teardown deliberately does not live in an `ExecStop` on import-tank.  A
  # service's stop job runs in the reverse of its start order, and that unit
  # starts after `local-fs.target` -- so it stops *before* the mounts sitting on
  # the pool come down.  The `zpool export tank` that used to be there therefore
  # ran against a live system: it spent a minute pulling datasets out from under
  # services systemd still believed were mounted, failed with "pool is busy"
  # anyway, and handed the final shutdown phase a pool that was still imported
  # over three dm-crypt mappings that then could not be closed either.
  #
  # A shutdown-ramfs hook is the one place this can succeed, because the copy of
  # it that matters runs after systemd-shutdown has pivoted into the ramfs and
  # unmounted everything.  systemd-shutdown walks that directory in both passes
  # -- once from the real root before the pivot, once from the ramfs after it --
  # and only the second one has anything in it, because nothing populates
  # /etc/systemd/system-shutdown on the real root and the `mkForce` below
  # reaches only the ramfs.  Do not assume single execution if that changes.
  #
  # It replaces the hook the nixpkgs ZFS module installs at this same path
  # rather than sitting beside it: systemd runs everything in that directory in
  # *parallel*, so a second script racing `zpool sync` for the pool namespace
  # would be worse than superseding it -- and an export syncs on its way out.
  # systemd caps the whole directory at 90s, so a wedge in here cannot cost more
  # than that.
  #
  # The upshot is that there is now no supported way to put tank away short of a
  # reboot: `systemctl stop import-tank` leaves the pool imported while systemd
  # believes the unit is inactive.  Use `zpool export tank` by hand.
  systemd.shutdownRamfs = {
    contents."/etc/systemd/system-shutdown/zpool".source = lib.mkForce (
      pkgs.writeShellScript "export-pools-shutdown" ''
        # There is no udevd here -- systemd-shutdown SIGKILLed it long before
        # this runs -- but `switch_root` carries the old /run across with a
        # stale /run/udev/control still in it, which is what libdevmapper reads
        # to decide whether to synchronise.  If it reads that as "udev is up",
        # the cookie wait below has no timeout of its own and eats the whole
        # 90s budget.
        export DM_DISABLE_UDEV=1

        ${zfsPackage}/bin/zpool export -a || true

        # Anything that could not be exported -- tank-backup left imported by
        # hand, say -- still wants its transaction group on disk, which is all
        # the hook this replaces ever did.
        ${zfsPackage}/bin/zpool sync || true

        ${lib.concatMapStringsSep "\n" (
          opts: "${pkgs.cryptsetup}/bin/cryptsetup close crypt-${opts.filenameBase} || true"
        ) (builtins.attrValues config.detachedLuksWithNixopsKeys)}
      ''
    );

    # Both of these have to be named.  make-initrd-ng follows ELF dependencies,
    # symlinks and directory entries, but it never reads a script for store
    # references -- so a binary a hook only mentions in its text is simply
    # absent at shutdown, and the line fails `command not found`.
    # `zpool` would otherwise ride along on the ZFS module's own storePaths,
    # which the `mkForce` above does not displace, but depending on that is
    # depending on a line of nixpkgs one below the thing being overridden.
    storePaths = [
      "${zfsPackage}/bin/zpool"
      "${pkgs.cryptsetup}/bin/cryptsetup"
    ];
  };

  boot = {
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    initrd = {
      availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" "e1000e" "igb"];
      kernelModules = ["dm-snapshot" "nls_cp437" "nls_iso8859_1"];
      luksWithKeyDrive."crypt-${root-disk-id}" = {
        device = "/dev/disk/by-id/${root-disk-id}";
        key = {
          drive = {inherit (config.fileSystems."/boot") device fsType;};
          file = "crypt/${root-disk-id}/key";
          header = "crypt/${root-disk-id}/header";
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
        device = "/dev/disk/by-uuid/4641-BCB3";
        fsType = "vfat";
      };
    }
    // (
      zfs.mkZfsFileSystems {
        "tank-fast/nix".mountpoint = "/nix";
        "tank-fast/log".mountpoint = "/var/log";
        "tank-fast/secrets" = {
          mountpoint = "/secrets";
          neededForBoot = true;
        };
      }
    );

  swapDevices = [];

  nix.settings.max-jobs = lib.mkDefault 16;

  services.xserver.videoDrivers = ["nvidia"];
  hardware = {
    graphics.enable = true;
    nvidia = {
      open = false;
      modesetting.enable = true;
      nvidiaSettings = false;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };
  };
}
