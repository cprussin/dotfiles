{ ... }:

let
  umask = "umask 077";
in

{
  primary-user.home-manager = {
    home.file = {
      ".profile".text = umask;
      ".bashrc".text = umask;
    };
    programs.zsh.initExtra = umask;
  };
}
