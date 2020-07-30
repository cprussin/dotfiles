{ config, pkgs, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in

{
  services.borgbackup.jobs.crux = {
    paths = map (folder: "/tank/${folder}/.zfs/snapshot/borgsnap") [
      "/"
      "/Library"
      "/Library/Movies"
      "/Library/Music"
      "/Library/Photos"
      "/Library/Software"
      "/Library/TV Shows"
      "/Users"
      "/Users/cprussin"
      "/Users/cprussin/Backups"
      "/Users/cprussin/Camera"
      "/Users/cprussin/Notes"
      "/Users/cprussin/Projects"
      "/Users/cprussin/Scratch"
    ];
    repo = "11795@ch-s011.rsync.net:crux-bak";
    encryption = {
      mode = "keyfile";
      passCommand = "cat ${config.deployment.keys.borgbackup.path}";
    };
    environment.BORG_RSH = "ssh -i ${config.deployment.keys."rsync.net-ssh".path}";
    compression = "auto,lzma";
    startAt = "hourly";
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
