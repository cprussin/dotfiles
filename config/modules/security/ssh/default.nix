_: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    controlMaster = "auto";
    controlPersist = "1m";
    matchBlocks.rsync = {
      user = "zh2593";
      hostname = "zh2593.rsync.net";
    };
  };
}
