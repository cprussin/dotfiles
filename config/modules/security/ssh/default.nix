{ ... }:

{
  primary-user.home-manager = {
    home.file.".ssh/known_hosts".source = ./known_hosts;

    programs.ssh = {
      enable = true;
      matchBlocks = {
        "crux lyra orion" = {
          checkHostIP = false;
        };
        "prussin.net *.bci-incorporated.com" = {
          user = "connor";
          port = 3580;
        };
      };
    };
  };
}
