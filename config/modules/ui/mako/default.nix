{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.mako];

    services.mako = {
      enable = true;
      settings = {
        border-size = 3;
        padding = "20";
        margin = "30";
        width = 500;
        height = 600;
        default-timeout = 10000;
      };
    };
  };
}
