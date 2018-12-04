{ ... }:

let
  umask = "umask 077";
in

{
  home.file.".profile".text = umask;
  home.file.".bashrc".text = umask;
  programs.zsh.initExtra = umask;
}
