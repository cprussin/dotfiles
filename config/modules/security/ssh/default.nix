{...}: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    matchBlocks = {
      "gemini crux mom-vm".forwardAgent = true;
      rsync = {
        user = "zh2593";
        hostname = "zh2593.rsync.net";
      };
    };
  };
}
