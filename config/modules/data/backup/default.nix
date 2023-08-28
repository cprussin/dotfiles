{
  pkgs,
  lib,
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

    if [ ! -e /dev/disk/by-id/wwn-0x5000cca0b0c4d39c ]
    then
      echo -n "Waiting for external drive to appear..."
      while [ ! -e /dev/disk/by-id/wwn-0x5000cca0b0c4d39c ]
      do
        echo -n '.'
        sleep 0.5
      done
      echo
    fi
    TEMP=$(mktemp -d)
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/wwn-0x5000cca0b0c4d39c/header > $TEMP/header.img
    ${pkgs.pass}/bin/pass show Connor/Infrastructure/luks/crux/wwn-0x5000cca0b0c4d39c/key |\
      sudo cryptsetup open --key-file - --header $TEMP/header.img /dev/disk/by-id/wwn-0x5000cca0b0c4d39c wwn-0x5000cca0b0c4d39c
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
    sudo cryptsetup close wwn-0x5000cca0b0c4d39c
  '';
in {
  primary-user.home-manager.home.packages = lib.mkForce [
    mountRsyncBackup
    umountRsyncBackup
    mountExternalBackup
    umountExternalBackup
  ];
}
