{ pkgs, lib, ... }:

let
  ls = pkgs.callPackage ./ls.nix { };
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  home.packages = lib.mkForce [ pkgs.starship ];

  programs = {
    zsh = {
      enable = true;
      shellAliases = {
        ls = toString ls;
        clear = toString clear;
      };
      defaultKeymap = "viins";
      autocd = true;
      initExtra = ''
        chpwd() {
          ${ls}
        }

        autoload -U colors
        colors

        # Turn on case-insensitive completion
        zstyle ':completion:*' matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

        setopt appendhistory
        setopt autopushd pushdtohome # Shortcuts to make cd easier
        setopt beep                  # Beep for me
        setopt noflowcontrol         # Disable annoying useless flow control
        setopt noclobber             # Keep me from overwriting files accidentally
        unsetopt notify              # Don't print status of background jobs until a prompt is about to be printed

        . ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

        [ "$(whoami)" = "root" ] || ${clear}
      '';
    };
    starship = {
      enable = true;
      settings.directory.truncate_to_repo = false;
    };
  };
}
