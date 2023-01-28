_: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    controlMaster = "auto";
    matchBlocks = {
      printotron.user = "pi";
      rsync = {
        user = "zh2593";
        hostname = "zh2593.rsync.net";
      };
    };
  };
}
