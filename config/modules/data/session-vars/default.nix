{...}: {
  primary-user.home-manager.imports = [./home.nix];
  home-manager.users.root.imports = [./home.nix];
}
