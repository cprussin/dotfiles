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

    if [ ! -e /dev/disk/by-id/${config.backupDiskId} ]
    then
      echo -n "Waiting for external drive to appear..."
      while [ ! -e /dev/disk/by-id/${config.backupDiskId} ]
      do
        echo -n '.'
        sleep 0.5
      done
      echo
    fi
    TEMP=$(mktemp -d)
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/${config.backupDiskId}/header > $TEMP/header.img
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/${config.backupDiskId}/key |\
      sudo cryptsetup open --key-file - --header $TEMP/header.img /dev/disk/by-id/${config.backupDiskId} ${config.backupDiskId}
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
    rmdir ~/.bak
    sudo zpool export tank-backup
    sudo cryptsetup close ${config.backupDiskId}
  '';
in {
  backupDiskId = "usb-WD_Elements_2621_575836324436334A5A325659-0:0";

  primary-user.home-manager.home.packages = lib.mkForce [
    mountRsyncBackup
    umountRsyncBackup
    mountExternalBackup
    umountExternalBackup
  ];
}
