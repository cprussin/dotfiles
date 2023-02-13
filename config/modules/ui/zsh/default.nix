{pkgs, ...}: {
  home-manager.users.root.imports = [./home.nix];

  primary-user = {
    home-manager.imports = [./home.nix];
    shell = "${pkgs.zsh}/bin/zsh";
  };

  # This is required to enable completions to be linked into the profile
  environment.pathsToLink = ["/share/zsh"];
}
