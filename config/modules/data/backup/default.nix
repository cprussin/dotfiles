{
  pkgs,
  lib,
  config,
  ...
}: let
  mountRsyncBackup = pkgs.writeShellScriptBin "mount-rsync-backup" ''
    set -e

    export BORG_PASSPHRASE=$(${pkgs.pass}/bin/pass show Connor/Infrastructure/borgbackup/crux/rsync.net)
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/borgbackup/crux/rsync.net\ key |\
      ${pkgs.borgbackup}/bin/borg key import --remote-path=borg1 "zh2593@zh2593.rsync.net:crux-bak" -
    ${pkgs.borgbackup}/bin/borg list --remote-path=borg1 "zh2593@zh2593.rsync.net:crux-bak"
    export ARCHIVE=$(${pkgs.borgbackup}/bin/borg list --last 1 --short --remote-path=borg1 "zh2593@zh2593.rsync.net:crux-bak")
    mkdir ~/.bak
    ${pkgs.borgbackup}/bin/borg mount --remote-path=borg1 -o uid=$UID,umask=077 "zh2593@zh2593.rsync.net:crux-bak::$ARCHIVE" ~/.bak
    echo
    echo "Mounted rysnc backup to ~/.bak"
  '';

  umountRsyncBackup = pkgs.writeShellScriptBin "umount-rsync-backup" ''
    set -e

    ${pkgs.borgbackup}/bin/borg umount --remote-path=borg1 ~/.bak
    rmdir ~/.bak
    shred -u ~/.config/borg/keys/*
    rm -rf ~/.config/borg ~/.cache/borg
  '';

  mountExternalBackup = pkgs.writeShellScriptBin "mount-external-backup" ''
    set -e

    if [ ! -e /dev/disk/by-id/${config.backupDisk.diskId} ]
    then
      echo -n "Waiting for external drive to appear..."
      while [ ! -e /dev/disk/by-id/${config.backupDisk.diskId} ]
      do
        echo -n '.'
        sleep 0.5
      done
      echo
    fi
    TEMP=$(mktemp -d)
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/${config.backupDisk.filenameBase}/header > $TEMP/header.img
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/${config.backupDisk.filenameBase}/key |\
      sudo cryptsetup open --key-file - --header $TEMP/header.img /dev/disk/by-id/${config.backupDisk.diskId} crypt-${config.backupDisk.filenameBase}
    sudo zpool import -o readonly=on tank-backup -R /tank-backup
    mkdir ~/.bak
    sudo ${pkgs.bindfs}/bin/bindfs --perms=0400,u+X --force-user=cprussin /tank-backup /home/cprussin/.bak
    shred -u $TEMP/header.img
    rm -rf $TEMP
    echo
    echo "Mounted external backup to ~/.bak"
  '';

  umountExternalBackup = pkgs.writeShellScriptBin "umount-external-backup" ''
    set -e

    sudo umount /home/cprussin/.bak
    rmdir /home/cprussin/.bak

    # `umount` only detaches the mount from the namespace and returns.  bindfs,
    # the FUSE daemon serving it, is asked to shut down asynchronously and keeps
    # /tank-backup as its working directory until it actually exits -- a live
    # cwd on the dataset is exactly what keeps it from being unmounted.
    # Exporting into that window is what failed with "pool is busy", so wait
    # the daemon out rather than racing it.
    EXPORTED=no
    WAITING=no
    for _ in {1..30}
    do
      if ERROR=$(sudo zpool export tank-backup 2>&1)
      then
        EXPORTED=yes
        break
      elif [ "$WAITING" = no ]
      then
        WAITING=yes
        echo -n "Waiting for the pool to be released..."
      fi
      echo -n '.'
      sleep 0.5
    done

    if [ "$WAITING" = yes ]
    then
      echo
    fi

    # Say what is left up rather than suggesting a re-run: the umount above has
    # already happened, so running this again just dies on the missing
    # mountpoint.  The pool matters more than the mapping -- run-backup
    # refuses to run at all while tank-backup is imported.
    if [ "$EXPORTED" = no ]
    then
      echo "$ERROR" >&2
      echo "WARNING: tank-backup is still imported." >&2
      echo "WARNING: leaving crypt-${config.backupDisk.filenameBase} open." >&2
      echo "Once the pool is free, finish with:" >&2
      echo "  sudo zpool export tank-backup &&" >&2
      echo "    sudo cryptsetup close crypt-${config.backupDisk.filenameBase}" >&2
      exit 1
    fi

    sudo cryptsetup close crypt-${config.backupDisk.filenameBase}
  '';
in {
  backupDisk.diskId = "usb-WD_Elements_2621_575836324436334A5A325659-0:0";

  primary-user.home-manager.home.packages = lib.mkForce [
    mountRsyncBackup
    umountRsyncBackup
    mountExternalBackup
    umountExternalBackup
  ];
}
