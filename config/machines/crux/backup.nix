{ config, pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  rsyncUser = "11795";
  rsyncHost = "ch-s011.rsync.net";
  userAtHost = "${rsyncUser}@${rsyncHost}";
in

{
  services.borgbackup.jobs."rsync.net" = {
    paths = map (folder: "${folder}/.zfs/snapshot/borgsnap") [
      "/srv/Library/Movies"
      "/srv/Library/Music"
      "/srv/Library/Photos"
      "/srv/Library/Software"
      "/srv/Library/TV Shows"
      "/home/cprussin/Backups"
      "/home/cprussin/Camera"
      "/home/cprussin/Notes"
      "/home/cprussin/Projects"
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
    borgbackup.text = passwords.get-password "Infrastructure/borgbackup/crux/rsync.net";
    borgbackup-keyfile.text = passwords.get-full-password "Infrastructure/borgbackup/crux/rsync.net key";
    "rsync.net-ssh".text = passwords.get-full-password "Infrastructure/ssh/${userAtHost}";
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
      ];
      wants = [
        "borgbackup-job-rsync.net-import-keyfile.service"
        "borgbackup-key.service"
        "rsync.net-ssh-key.service"
      ];
    };
  };
}
