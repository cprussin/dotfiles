{ pkgs, lib, config, ... }:

let
  ls = pkgs.callPackage ./ls.nix { };
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  home.file.".zsh-dircolors.config".text = "dircolors.ansi-dark";

  programs.zsh = {
    enable = true;
    plugins = [
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.zsh-syntax-highlighting;
        file = "share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh";
      }
      {
        name = "zsh-git-prompt";
        src = pkgs.callPackage ./zsh-git-prompt.nix { };
        file = "zshrc.sh";
      }
      {
        name = "zsh-dircolors-solarized";
        src = pkgs.fetchFromGitHub {
          owner = "joel-porquet";
          repo = "zsh-dircolors-solarized";
          rev = "2d41598785ac7035c3f34a6beaba9c0973ab15e7";
          sha256 = "1xdq1qsccggbdzv562acc88ra6cfwjwbvggcnkjpqyg1817gp2q0";
          fetchSubmodules = true;
        };
      }
    ];
    shellAliases = {
      ls = ls;
      clear = clear;
      ghci = "${pkgs.ghc}/bin/ghci";
      node = "${pkgs.nodejs}/bin/node";
      open = "${pkgs.launcher}/bin/open";
      tree = "${pkgs.tree}/bin/tree";
      zip = "${pkgs.zip}/bin/zip";
      unzip = "${pkgs.unzip}/bin/unzip";
      file = "${pkgs.file}/bin/file";
      git = "${pkgs.git}/bin/git";
      man = "${pkgs.man}/bin/man";
      setterminfo = "${pkgs.setterminfo}/bin/setterminfo";
      pass = "${pkgs.pass}/bin/pass";
      nixops = "${pkgs.nixops}/bin/nixops";
      xmonad = config.xsession.windowManager.command;
    };
    initExtra = ''
      shell() {
        nix-shell --run "$*"
      }

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

      ${clear}
    '';
  };
}
