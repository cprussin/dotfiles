{ pkgs, config, ... }:

let
  ls = pkgs.callPackage ./ls.nix {};
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  users.users.${config.primaryUserName}.shell = "${pkgs.zsh}/bin/zsh";

  home-manager.users.${config.primaryUserName}.programs.zsh = {
    enable = true;
    shellAliases = {
      ls = toString ls;
      clear = toString clear;
    };
    initExtra = ''
      chpwd() {
          ${ls}
      }

      autoload -U colors
      colors
      ZSH_THEME_GIT_PROMPT_PREFIX=" ["
      ZSH_THEME_GIT_PROMPT_SHOW_UPSTREAM=1
      PROMPT='%F{cyan}%M %~%F{reset}$(git_super_status) %(?..%F{red}[%?]%F{reset} )$ '

      bindkey -v

      # Turn on case-insensitive completion
      zstyle ':completion:*' matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

      setopt appendhistory
      setopt autocd autopushd pushdtohome # Shortcuts to make cd easier
      setopt beep                         # Beep for me
      setopt noflowcontrol                # Disable annoying useless flow control
      setopt noclobber                    # Keep me from overwriting files accidentally
      unsetopt notify                     # Don't print status of background jobs until a prompt is about to be printed

      eval $(dircolors ${config.colorTheme.dircolors})

      . ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      . ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zsh-git-prompt.zsh

      ${clear}
    '';
  };
}
