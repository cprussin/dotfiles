{ pkgs, ... }:

{
  primary-user.home-manager.iconTheme = {
    package = pkgs.papirus-icon-theme;
    name = "Papirus";
  };
}
