{ config, pkgs, ... }:
let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
  rsyncUser = "11795";
  rsyncHost = "ch-s011.rsync.net";
  userAtHost = "${rsyncUser}@${rsyncHost}";
in
{
  services.borgbackup.jobs."rsync.net" = {
    paths = map (folder: "${folder}/.zfs/snapshot/borgsnap") [
      "/home/cprussin/Backups"
      "/home/cprussin/Camera"
      "/home/cprussin/Notes"
      "/home/cprussin/Phone"
      "/home/cprussin/Projects"
      "/srv/Library/Family Photos"
      "/srv/Library/Media Library/Movies"
      "/srv/Library/Media Library/Music"
      "/srv/Library/Media Library/TV Shows"
      "/srv/Library/ROMs"
      "/srv/Library/Software Library"
      "/var/lib/hass"
      "/var/lib/matrix-synapse"
      "/var/lib/postgresql"
    ];
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
    preHook = "${pkgs.zfs}/bin/zfs snapshot -r tank@borgsnap";
    postHook = "${pkgs.zfs}/bin/zfs destroy -r tank@borgsnap";
    extraArgs = "--remote-path=borg1";
  };

  deployment.keys = {
    borgbackup.keyCommand = passwords.getPassword "Infrastructure/borgbackup/crux/rsync.net";
    borgbackup-keyfile.keyCommand = passwords.getFullPassword "Infrastructure/borgbackup/crux/rsync.net key";
    "rsync.net-ssh".keyCommand = passwords.getFullPassword "Infrastructure/ssh/${userAtHost}";
  };

  systemd.services = {
    "borgbackup-job-rsync.net-import-keyfile" = {
      description = "Import the borgbackup keyfile for rsync.net.";
      after = [ "borgbackup-keyfile-key.service" ];
      wants = [ "borgbackup-keyfile-key.service" ];
      environment = config.services.borgbackup.jobs."rsync.net".environment;
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
      wants = [
        "borgbackup-job-rsync.net-import-keyfile.service"
        "borgbackup-key.service"
        "rsync.net-ssh-key.service"
        "import-tank.service"
      ];
    };
  };

  programs.ssh.knownHosts."ch-s011.rsync.net".publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEYEyoL8HADxd4D1md7t2LGcM8nNhShc5qCjttVH1vTg";
}
