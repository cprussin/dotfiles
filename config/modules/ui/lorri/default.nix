{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager = {
    services.lorri.enable = true;
    home.packages = lib.mkForce [pkgs.lorri];
  };
}
