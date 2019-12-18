{ pkgs, lib, ... }:

let
  fzf-preview = pkgs.writeShellScript "fzf-preview" ''
    bat=${pkgs.bat}/bin/bat
    head=${pkgs.coreutils}/bin/head
    ls=${pkgs.coreutils}/bin/ls
    figlet=${pkgs.figlet}/bin/figlet

    if [ -d "$1" ]
    then
      $ls --color=always -1 "$1" | $head -n 500
    elif [ -f "$1" ]
    then
      $bat --style=numbers --color=always "$1" | $head -n 500
    else
      $figlet -c "$1"
    fi
  '';
in

{
  primary-user.home-manager = {
    home.packages = lib.mkForce [ pkgs.fzf ];

    programs.fzf = {
      enable = true;
      inline-info = true;
      preview = "'${fzf-preview} {}'";
    };
  };
}
