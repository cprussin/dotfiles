let
  stateVersion = "23.11";
in
  _: {
    system.stateVersion = stateVersion;
    primary-user.home-manager.home.stateVersion = stateVersion;
    home-manager.users.root.home.stateVersion = stateVersion;
  }
