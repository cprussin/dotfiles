{ ... }:

{
  primary-user.home-manager = {
    home.file.".ssh/known_hosts".source = ./known_hosts;

    programs.ssh = {
      enable = true;
      matchBlocks = {
        "prussin.net *.bci-incorporated.com" = {
          user = "connor";
          port = 3580;
        };
      };
    };
  };
}
