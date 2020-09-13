{ config, pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in

{
  services.borgbackup.jobs.crux = {
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
    repo = "11795@ch-s011.rsync.net:crux-bak";
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
    borgbackup.text = passwords.get-password "Infrastructure/borg/crux";
    "rsync.net-ssh".text = passwords.get-full-password "Infrastructure/ssh/11795@ch-s011.rsync.net";
  };

  systemd.services.borgbackup-job-crux = {
    after = [ "borgbackup-key.service" "rsync.net-ssh-key.service" ];
    wants = [ "borgbackup-key.service" "rsync.net-ssh-key.service" ];
  };
}
