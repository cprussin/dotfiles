{
  lib,
  pkgs,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.git];

    programs.git = {
      enable = true;
      settings.user = {
        name = "Connor Prussin";
        email = "connor@prussin.net";
        pull.rebase = true;
        color.ui = "auto";
        push = {
          default = "simple";
          gpgsign = "if-asked";
        };
      };
      signing = {
        key = "0x426ABF93ACE024D0";
        signByDefault = true;
      };
    };
  };
}
