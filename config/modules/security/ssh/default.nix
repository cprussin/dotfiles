{...}: {
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    matchBlocks = {
      crux = {
        checkHostIP = false;
        hostname = "crux.prussin.net";
      };
      "lyra gemini orion" = {
        checkHostIP = false;
      };
      rsync = {
        user = "11795";
        hostname = "ch-s011.rsync.net";
      };
      McPuter = {
        hostname = "localhost";
        proxyJump = "crux";
        port = 8555;
      };
    };
  };
}
