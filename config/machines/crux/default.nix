{ pkgs, config, ... }:

let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
in

{
  imports = [
    ./hardware.nix
    ../../profiles/physical-machine
    ../../profiles/server
    ../../modules/data/syncthing
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };

  services = {
    mingetty.greetingLine = builtins.readFile ./greeting;

    borgbackup.jobs.crux = {
      paths = [
        "/tank/.zfs/snapshot/borgsnap"
        "/tank/Users/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Backups/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Camera/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Music/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Notes/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Projects/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Scratch/.zfs/snapshot/borgsnap"
        "/tank/Users/cprussin/Software/.zfs/snapshot/borgsnap"
      ];
      repo = "11795@ch-s011.rsync.net:crux-bak";
      encryption = {
        mode = "keyfile";
        passCommand = "cat ${config.deployment.keys.borgbackup.path}";
      };
      environment.BORG_RSH = "ssh -i ${config.deployment.keys."rsync.net-ssh".path}";
      compression = "auto,lzma";
      startAt = "daily";
      prune.keep = {
        within = "1d";
        daily = 7;
        weekly = 4;
        monthly = -1;
      };
      preHook = "${pkgs.zfs}/bin/zfs snapshot -r tank@borgsnap";
      postHook = "${pkgs.zfs}/bin/zfs destroy -r tank@borgsnap";
    };
  };

  deployment.keys = {
    borgbackup.text = passwords.get-password "Infrastructure/borg/crux";
    "rsync.net-ssh".text = passwords.get-full-password "Infrastructure/ssh/11795@ch-s011.rsync.net";
  };
}
