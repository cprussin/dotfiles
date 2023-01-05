{
  config,
  pkgs,
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
        ${zfs} list -H -s name -o name,net.prussin:backup -t snapshot |\
        ${awk} -F '\t' '$1 ~ /@borgsnap/ && tolower($2) ~ /true/ {print $1}'
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
}
