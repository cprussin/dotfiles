{
  lib,
  config,
  ...
}: let
  userMountpoints = builtins.filter (lib.hasPrefix "/home") (
    builtins.attrNames config.fileSystems
  );

  installMountpoint = mountpoint: "mkdir -m 0700 -p ${mountpoint}";
in {
  users.mutableUsers = false;

  # TODO fix this to not suck
  #   Should we update mounts that start with /user instead?
  systemd.services.install-user-mountpoints = {
    wantedBy = ["local-fs.target"];
    wants = ["local-fs-pre.target"];
    before = ["local-fs-pre.target"];
    unitConfig.DefaultDependencies = false;
    script = ''
      ${builtins.concatStringsSep "\n" (map installMountpoint userMountpoints)}
      for homedir in /home/*
      do
        chown --recursive ''${homedir##/home/}:users $homedir
      done
    '';
    serviceConfig.Type = "oneshot";
  };
}
