{ pkgs, ... }:

{
  home-manager.users.root.imports = [ ./home.nix ];

  primary-user = {
    home-manager.imports = [ ./home.nix ];
    shell = "${pkgs.zsh}/bin/zsh";
  };
}
