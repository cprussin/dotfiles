{ pkgs, config, ... }:

let
  ls = pkgs.callPackage ./ls.nix {};
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  home-manager.users.${config.primaryUserName} = homeManager: {
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
          src = pkgs.callPackage ./zsh-git-prompt.nix {};
          file = "zshrc.sh";
        }
      ];
      shellAliases = {
        ls = toString ls;
        clear = toString clear;
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
        get-aws-access-key = "${pkgs.get-aws-access-key}/bin/get-aws-access-key";
        get-aws-access-key-nixops = "${pkgs.get-aws-access-key}/bin/get-aws-access-key-nixops";
        xmonad = homeManager.config.xsession.windowManager.command;
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

        ${clear}
      '';
    };
  };
}
