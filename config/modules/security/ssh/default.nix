{ ... }:

{
  primary-user.home-manager.programs.ssh = {
    enable = true;
    userKnownHostsFile = "${./known_hosts}";
    matchBlocks = {
      "crux" = {
        checkHostIP = false;
        hostname = "crux.prussin.net";
      };
      "lyra gemini orion" = {
        checkHostIP = false;
      };
      "prussin.net *.bci-incorporated.com" = {
        user = "connor";
        port = 3580;
      };
      "rsync" = {
        user = "11795";
        hostname = "ch-s011.rsync.net";
      };
    };
  };
}
