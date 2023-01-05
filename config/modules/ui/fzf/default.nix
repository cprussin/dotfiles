_: {
  home-manager.users.root.imports = [./home.nix];
  primary-user.home-manager.imports = [./home.nix];
}
