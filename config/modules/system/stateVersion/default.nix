let
  stateVersion = "25.11";
in
  _: {
    system.stateVersion = stateVersion;
    primary-user.home-manager.home.stateVersion = stateVersion;
    home-manager.users.root.home.stateVersion = stateVersion;
  }
