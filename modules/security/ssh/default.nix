{ pkgs, config, ... }:

{
  home-manager.users.${config.primaryUserName} = { ... }: {
    home.file.".ssh/known_hosts".source = ./known_hosts;
    home.file.".ssh/authorized_keys".source = ./authorized_keys;
    home.file.".ssh/environment".text = "PATH=${pkgs.git}/bin";

    programs.ssh = {
      enable = true;
      matchBlocks = [
        {
          host = "prussin.net *.bci-incorporated.com";
          user = "connor";
          port = 3580;
        }
        {
          host = "aur.archlinux.org";
          user = "aur";
        }
      ];
    };
  };
}
