{ pkgs, lib, config, ... }:

let
  ls = pkgs.callPackage ./ls.nix {};
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  options.promptColor = lib.mkOption {
    type = lib.types.str;
  };

  config.primary-user = {
    shell = "${pkgs.zsh}/bin/zsh";

    home-manager.programs.zsh = {
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
        ZSH_THEME_GIT_PROMPT_PREFIX=" ["
        ZSH_THEME_GIT_PROMPT_SHOW_UPSTREAM=1
        PROMPT='%F{${config.promptColor}}%M %~%F{reset}$(git_super_status) %(?..%F{red}[%?]%F{reset} )$ '

        # Turn on case-insensitive completion
        zstyle ':completion:*' matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

        setopt appendhistory
        setopt autopushd pushdtohome # Shortcuts to make cd easier
        setopt beep                  # Beep for me
        setopt noflowcontrol         # Disable annoying useless flow control
        setopt noclobber             # Keep me from overwriting files accidentally
        unsetopt notify              # Don't print status of background jobs until a prompt is about to be printed

        . ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        . ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zsh-git-prompt.zsh

        ${clear}
      '';
    };
  };
}
