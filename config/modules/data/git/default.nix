{
  lib,
  pkgs,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.git];

    programs.git = {
      enable = true;
      userName = "Connor Prussin";
      userEmail = "connor@prussin.net";
      signing = {
        key = "0x426ABF93ACE024D0";
        signByDefault = true;
      };
      extraConfig = {
        pull.rebase = true;
        color.ui = "auto";
        push = {
          default = "simple";
          gpgsign = "if-asked";
        };
      };
    };
  };
}
