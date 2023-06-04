{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.mako];

    services.mako = {
      enable = true;
      borderSize = 3;
      padding = "20";
      margin = "30";
      width = 500;
      height = 600;
      defaultTimeout = 10000;
    };
  };
}
