_: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    matchBlocks = {
      printotron.user = "pi";
      "gemini crux mom-vm".forwardAgent = true;
      rsync = {
        user = "zh2593";
        hostname = "zh2593.rsync.net";
      };
    };
  };
}
