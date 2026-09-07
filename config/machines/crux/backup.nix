{
  config,
  pkgs,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  rsyncUser = "zh2593";
  rsyncHost = "zh2593.rsync.net";
  userAtHost = "${rsyncUser}@${rsyncHost}";
  zfs = "${pkgs.zfs}/bin/zfs";
  awk = "${pkgs.gawk}/bin/awk";
  sed = "${pkgs.gnused}/bin/sed";
  grep = "${pkgs.gnugrep}/bin/grep";
  mkdir = "${pkgs.coreutils}/bin/mkdir";
  mount = "${config.security.wrapperDir}/mount";
  umount = "${config.security.wrapperDir}/umount";

  unlockBackupDisk = "unlock-${config.detachedLuksWithNixopsKeys."${config.backupDisk.diskId}".filenameBase}.service";

  # Nothing is ever mounted here -- the import below is `-N` and import-tank no
  # longer runs a pool-wide `zfs mount -a`.  The path exists to be the altroot:
  # a prefix every mountpoint on tank-backup is rewritten under, so that even a
  # `zfs mount -a` from somewhere this repo does not control (upstream's
  # zfs-mount.service, a hand-run command) cannot land a received dataset on
  # the internal path it was copied from.
  backupAltroot = "/run/tank-backup";

  runBackup = pkgs.writeShellScriptBin "run-backup" ''
    # pipefail because the step that matters most here is a pipeline: without
    # it a failed `zfs send` is masked by the `zfs recv` that read its
    # truncated output, and the script goes on to destroy the snapshot the
    # next incremental needs as its base.
    set -eo pipefail

    if [ $UID -ne 0 ]
    then
      echo "This script must be run with sudo" >&2
      exit 1
    fi

    if [ ! -e /dev/disk/by-id/${config.backupDisk.diskId} ]
    then
      echo -ne "\e[1;37mWaiting for external drive to appear...\e[0m"
      while [ ! -e /dev/disk/by-id/${config.backupDisk.diskId} ]
      do
        echo -n '.'
        sleep 0.5
      done
      echo
    fi

    echo
    # Refuse rather than take over.  mount-external-backup imports this same
    # pool readonly to browse it, through the same LUKS mapping name, so a run
    # that adopted an import it did not make would export the pool and close
    # the mapping out from under whoever was reading it.
    if zpool list tank-backup > /dev/null 2>&1
    then
      echo "tank-backup is already imported;" >&2
      echo "export it before running a backup." >&2
      exit 1
    fi

    echo -e "\e[1;37mMounting tank-backup...\e[0m"

    # The trap goes in before any of the state it undoes exists, rather than
    # after: a run that stopped in the middle left the pool imported, and that
    # was the landmine the next deploy stepped on.  Each step is gated on its
    # own flag so cleanup only ever undoes what this run actually did.
    UNLOCKED=no
    IMPORTED=no
    SNAPSHOT_TAKEN=no
    SENT=no

    cleanup() {
      echo
      echo -e "\e[1;37mCleaning up...\e[0m"

      # A run that took today's snapshot but never got it onto the disk would
      # leave two tank@external-backup-* snapshots behind, and the guard below
      # turns that into a hard stop on every later run -- so a transient USB
      # drop would wedge the backup until someone deleted a snapshot by hand.
      # This undoes only this run's own snapshot, and only while the receive
      # had not finished: any state this script did not create still stops the
      # next run for a person to look at.  `SENT=no` does not mean nothing
      # landed -- a `-R` stream commits one dataset at a time, so a drive that
      # drops mid-send can leave today's snapshot on the leading tank-backup
      # datasets and none on tank.  The next run's `zfs recv -F` destroys
      # destination snapshots the sender does not have, so that resolves
      # itself rather than needing a hand.
      if [ "$SNAPSHOT_TAKEN" = yes ] && [ "$SENT" = no ]
      then
        zfs destroy -r tank@external-backup-$TODAY ||
          echo "WARNING: tank@external-backup-$TODAY is still on tank" >&2
      fi

      if [ "$IMPORTED" = yes ] && ! zpool export tank-backup
      then
        # `exit` rather than falling through, for two reasons.  Falling through
        # runs `cryptsetup close` on a device the imported pool still holds, so
        # it fails too and its error buries this one; and a trap that returns
        # normally hands back the status the script already had, which would
        # report leaving the pool imported as a successful backup.
        echo "WARNING: tank-backup is still imported." >&2
        echo "WARNING: leaving ${unlockBackupDisk} up." >&2
        exit 1
      fi

      if [ "$UNLOCKED" = yes ]
      then
        systemctl stop ${unlockBackupDisk}
      fi
    }
    # The EXIT trap alone already runs cleanup when the script is signalled,
    # but bash then reports the *previous* command's status -- so a Ctrl-C
    # halfway through a scrub came out as exit 0, a backup that never happened
    # reported as one that did.  These two only fix the status; cleanup still
    # runs exactly once, from EXIT.
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap cleanup EXIT

    systemctl start ${unlockBackupDisk}
    UNLOCKED=yes

    # `-N` so the import mounts nothing, `-R` so that nothing else can either.
    # The datasets on this pool are received copies of tank's and carry tank's
    # mountpoints, /home/cprussin among them; an altroot rewrites all of them
    # to sit under it, which is what keeps the external disk off the internal
    # paths for the whole window the pool is imported.  `-R` also implies
    # `cachefile=none`, so a reboot in the middle of a backup cannot bring the
    # pool back on its own with no altroot set.
    zpool import -N -R ${backupAltroot} tank-backup
    IMPORTED=yes

    echo
    echo -e "\e[1;37mCreating backup snapshot...\e[0m"
    # Plain assignment rather than `export`: nothing here needs it in the
    # environment, and `export VAR=$(pipeline)` reports the *builtin's* status,
    # so pipefail would never see a failure inside the substitution.
    TODAY=$(date +%F)

    # Exactly one of these must exist.  It is both the base of the incremental
    # below and the snapshot this run retires at the end, so neither other
    # count has a right answer: none means there is nothing to send from and
    # the disk needs a full replication, and more than one means an earlier run
    # died between sending and retiring -- picking either would strand the
    # other on tank forever and send from a base the external disk may not
    # hold.  Both stop the run for a person to look at rather than getting
    # worked around here.
    #
    # Listed and filtered in two steps rather than one pipeline: `|| true` has
    # to be on the `grep`, so that finding nothing reaches the check below
    # instead of pipefail killing the script with nothing said about why.  On
    # the pipeline it would swallow a failing `zfs list` too, and answer "no
    # snapshots" -- pointing at a full replication -- when the real problem is
    # that tank is not there.
    TANK_SNAPS=$(zfs list -Ht snapshot -o name tank)
    LAST_SNAP=$(grep '^tank@external-backup-' <<< "$TANK_SNAPS" || true)
    if [ -z "$LAST_SNAP" ] || [ "$(wc -l <<< "$LAST_SNAP")" -ne 1 ]
    then
      echo "Expected exactly one tank@external-backup-* snapshot, found:" >&2
      echo "''${LAST_SNAP:-  (none)}" >&2
      exit 1
    fi

    zfs snapshot -r tank@external-backup-$TODAY
    SNAPSHOT_TAKEN=yes

    echo
    echo -e "\e[1;37mSending incremental data...\e[0m"
    # `-u` keeps the receive from mounting anything, and `-o readonly=on`
    # keeps anything that does get mounted -- by mount-external-backup, or by
    # hand -- from being written to.  `readonly` is an inheritable property and
    # `-o` on a `send -R` stream applies it to the whole received tree rather
    # than letting tank's own `readonly=off` come across with the properties.
    # Receives themselves are unaffected: they run below the ZPL, so the next
    # incremental still lands.
    zfs send -R -X tank/Cache,tank/Data/cprussin/Private,tank/Data/cprussin/Scratch,tank/Data/frigate -I "$LAST_SNAP" tank@external-backup-$TODAY | zfs recv -Fdu -o readonly=on tank-backup
    SENT=yes

    echo
    echo -e "\e[1;37mScrubbing tank-backup...\e[0m"

    # Every `zpool status` below is read by `grep`, and its output is
    # translated, so pin the locale: sudo keeps the caller's LANG, and under
    # one the gate at the end would reject every clean run.
    poolStatus() {
      LC_ALL=C zpool status tank-backup 2>&1
    }

    # Every `zpool status` from here on runs with the receive already done, so
    # a failure leaves today's snapshot on tank beside the base this run meant
    # to retire -- the two-snapshot state the guard above stops on.  Say what
    # that takes to clear, rather than dying with only "Cleaning up..." on the
    # screen.
    snapshotsLeft() {
      echo "$LAST_SNAP was not retired, so tank holds both it and" >&2
      echo "tank@external-backup-$TODAY -- which the next run refuses" >&2
      echo "to choose between.  Once tank-backup is readable, stop any" >&2
      echo "scan still on it and destroy one of the two by hand." >&2
      exit 1
    }

    scrubUnknown() {
      echo "Scrub result unknown." >&2
      snapshotsLeft
    }

    STATUS=$(poolStatus) || {
      echo "$STATUS" >&2
      scrubUnknown
    }

    # A scan already running has to be stopped rather than joined.  The pool
    # resumes an interrupted scan when it is imported, so a run cut short
    # during its scrub leaves one behind -- and `zpool scrub` on a pool already
    # scanning is an error, which would kill the next run right here.  Joining
    # it would not do either: it started before this run's receive, so it has
    # already read past whatever just landed.
    #
    # Paused counts as running.  `zpool scrub` on a paused scan *resumes* it
    # and returns 0 rather than erroring, so that one does not announce itself
    # -- the loop below would wait out a scan that never read this run's data
    # and the gate at the end would pass it.
    #
    # `|| true` for the race where it finishes on its own in between, which
    # leaves nothing to stop and is not a failure.
    if grep -qE 'scrub (in progress|paused) ' <<< "$STATUS"
    then
      echo "Stopping a scrub left over from an earlier run..."
      zpool scrub -s tank-backup || true
      STATUS=$(poolStatus) || {
        echo "$STATUS" >&2
        scrubUnknown
      }

      # Checked rather than assumed, because `|| true` above cannot tell the
      # race it is there for from a stop that genuinely failed.  A scan that
      # survived would be resumed by the `zpool scrub` below -- which returns
      # 0 for that -- and then passed off as this run's own.
      if grep -qE 'scrub (in progress|paused) ' <<< "$STATUS"
      then
        echo "A scan is still running on tank-backup after trying to" >&2
        echo "stop it, so this run cannot tell its own scrub from that" >&2
        echo "one." >&2
        snapshotsLeft
      fi
    fi

    # The scan line the pool already carries, so the gate at the end compares
    # against it rather than trusting whatever is there when the loop ends.
    # Its other two conditions ask whether that line describes a clean
    # completed scrub; this one asks whether it is a new line at all, which is
    # what keeps a result the pool was already carrying -- a clean scrub from
    # last week among them -- from being read off as this run's own.
    SCAN_BEFORE=$(sed -n 's/^  scan: //p' <<< "$STATUS")

    zpool scrub tank-backup || {
      echo "Could not start a scrub." >&2
      snapshotsLeft
    }

    exec 3>&1
    DRAWN=0
    clearProgress() {
      if [ -t 1 ] && [ "$DRAWN" -gt 0 ]
      then
        printf '\033[%dA\033[J' "$DRAWN" || true
        DRAWN=0
      fi
    }
    while true
    do
      # Fatal rather than a `break`: this loop is the only thing that ever
      # learns how the scrub went, and it used to leave $STATUS unset here and
      # carry on to retire the base snapshot on a result nobody had.
      NEXT=$(poolStatus) || {
        clearProgress
        echo "$NEXT" >&2
        scrubUnknown
      }
      STATUS=$NEXT
      clearProgress
      grep -q 'scrub in progress' <<< "$STATUS" || break
      if [ -t 1 ]
      then
        WIDTH=$(tput cols <&3 2>/dev/null || echo 80)
        PROGRESS=$(echo "$STATUS" | sed -n '/scan:/,/^config:/{/^config:/!p}' | expand | cut -c "1-$WIDTH")
        echo "$PROGRESS"
        DRAWN=$(echo "$PROGRESS" | wc -l)
      fi
      sleep 1
    done
    exec 3>&-
    echo "$STATUS"

    # The scrub is the whole reason to trust what is on the external disk, and
    # the snapshot retired below is the base the *next* incremental rests on,
    # so nothing short of "this run's scrub finished and found nothing" may
    # retire it.  Three conditions, because no one of them says that alone:
    # the scan line has to have moved, or it is still the previous scrub's; it
    # has to read as a completed clean scrub, or the run was cancelled, paused
    # or resilvered instead; and the pool's error log has to be empty, which
    # covers errors found outside the scan.
    SCAN_AFTER=$(sed -n 's/^  scan: //p' <<< "$STATUS")
    if [ "$SCAN_AFTER" = "$SCAN_BEFORE" ] ||
      ! grep -q '^scrub repaired .* with 0 errors on ' <<< "$SCAN_AFTER" ||
      ! grep -q '^errors: No known data errors$' <<< "$STATUS"
    then
      echo "tank-backup did not scrub clean." >&2
      echo "  scan before: ''${SCAN_BEFORE:-(none)}" >&2
      echo "  scan after:  ''${SCAN_AFTER:-(none)}" >&2
      snapshotsLeft
    fi

    echo
    echo -e "\e[1;37mRetiring the previous snapshot...\e[0m"
    zfs destroy -r "$LAST_SNAP"
  '';
in {
  services.borgbackup.jobs."rsync.net" = {
    paths = ["/tank"];
    repo = "${userAtHost}:crux-bak";
    encryption = {
      mode = "keyfile";
      passCommand = "cat ${config.deployment.keys.borgbackup.path}";
    };
    environment.BORG_RSH = "ssh -i ${config.deployment.keys."rsync.net-ssh".path}";
    compression = "auto,lzma";
    startAt = "*:0/15";
    prune.keep = {
      within = "1d";
      daily = 7;
      weekly = 4;
      monthly = -1;
    };
    preHook = ''
      ${zfs} snapshot -r tank@borgsnap
      IFS="
      "
      getSnaps() {
        ${zfs} list -H -s name -o name,net.prussin:backup -t snapshot -r tank |\
        ${awk} -F '\t' '$1 ~ /@borgsnap$/ && tolower($2) ~ /true/ {print $1}'
      }
      for snap in $(getSnaps)
      do
        target="/''${snap%%@*}"
        ${mkdir} -p "$target"
        ${mount} -t zfs "$snap" "$target"
      done
    '';
    postHook = ''
      getSnaps() {
        ${mount} |\
        ${grep} "on /tank/" |\
        ${sed} 's|@borgsnap on /tank/.*|@borgsnap|'
      }
      for snap in $(getSnaps)
      do
        ${umount} "$snap"
      done
      ${zfs} destroy -r tank@borgsnap
    '';
    extraArgs = "--remote-path=borg1";
    exclude = [
      "**/node_modules/**"
      "**/target/**"
      "**/.pnpm-store/**"
      "**/.direnv/**"
      "**/.turbo/**"
      "**/dist/**"
      "**/worktrees/**"
      "**/.worktrees/**"
    ];
  };

  deployment.keys = {
    borgbackup.keyCommand = passwords.getPassword "Connor/Infrastructure/borgbackup/crux/rsync.net";
    borgbackup-keyfile.keyCommand = passwords.getFullPassword "Connor/Infrastructure/borgbackup/crux/rsync.net key";
    "rsync.net-ssh".keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssh/${userAtHost}";
  };

  systemd.services = {
    "borgbackup-job-rsync.net-import-keyfile" = {
      inherit (config.services.borgbackup.jobs."rsync.net") environment;
      description = "Import the borgbackup keyfile for rsync.net.";
      after = [
        "borgbackup-keyfile-key.service"
        "rsync.net-ssh-key.service"
      ];
      requires = [
        "borgbackup-keyfile-key.service"
        "rsync.net-ssh-key.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = builtins.concatStringsSep " " [
          "${pkgs.borgbackup}/bin/borg key import"
          config.services.borgbackup.jobs."rsync.net".extraArgs
          "\"${config.services.borgbackup.jobs."rsync.net".repo}\""
          config.deployment.keys.borgbackup-keyfile.path
        ];
      };
    };

    "borgbackup-job-rsync.net" = {
      after = [
        "borgbackup-job-rsync.net-import-keyfile.service"
        "borgbackup-key.service"
        "rsync.net-ssh-key.service"
        "import-tank.service"
      ];
      requires = [
        "borgbackup-job-rsync.net-import-keyfile.service"
        "borgbackup-key.service"
        "rsync.net-ssh-key.service"
        "import-tank.service"
      ];
      serviceConfig.TemporaryFileSystem = ["/tank"];
    };
  };

  programs.ssh.knownHosts."zh2593.rsync.net".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJtclizeBy1Uo3D86HpgD3LONGVH0CJ0NT+YfZlldAJd";

  primary-user.home-manager.home.packages = lib.mkForce [runBackup];
}
