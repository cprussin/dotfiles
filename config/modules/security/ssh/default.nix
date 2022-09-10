{...}: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    matchBlocks = {
      "gemini crux" = {
        checkHostIP = false;
        forwardAgent = true;
      };
      rsync = {
        user = "11795";
        hostname = "ch-s011.rsync.net";
      };
    };
  };
}
