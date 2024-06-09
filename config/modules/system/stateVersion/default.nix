let
  stateVersion = "24.05";
in
  _: {
    system.stateVersion = stateVersion;
    primary-user.home-manager.home.stateVersion = stateVersion;
    home-manager.users.root.home.stateVersion = stateVersion;
  }
