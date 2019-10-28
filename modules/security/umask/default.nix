{ config, ... }:

let
  umask = "umask 077";
in

{
  home-manager.users.${config.primaryUserName} = {
    home.file = {
      ".profile".text = umask;
      ".bashrc".text = umask;
    };
    programs.zsh.initExtra = umask;
  };
}
