_: {
  home-manager.users.root.imports = [./home.nix];
  primary-user.home-manager.imports = [./home.nix];

  # This is required to enable completions to be linked into the profile
  environment.pathsToLink = ["/share/zsh"];
}
