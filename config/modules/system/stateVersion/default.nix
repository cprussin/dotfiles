{...}: let
  stateVersion = "22.11";
in {
  system.stateVersion = stateVersion;
  primary-user.home-manager.home.stateVersion = stateVersion;
  home-manager.users.root.home.stateVersion = stateVersion;
}
