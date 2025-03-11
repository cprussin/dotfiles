{
  pkgs,
  lib,
  ...
}: {
  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.starship];
    programs.starship = {
      enable = true;
      settings.directory.truncate_to_repo = false;
    };
  };
}
