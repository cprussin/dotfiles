{ pkgs, lib, ... }:

let
  ls = pkgs.callPackage ./ls.nix { };

  clear = pkgs.callPackage ./clear.nix { inherit ls; };

  mkNixShellAlias = name: pkg: ''
    ${name}() {
      nix-shell -p ${pkg} --run "${name} $*"
    }
  '';

  mkNixShellAliases = aliases:
    lib.concatStringsSep "\n" (lib.mapAttrsToList mkNixShellAlias aliases);
in

{
  home.file.".zsh-dircolors.config".text = "dircolors.ansi-dark";

  nixpkgs.overlays = [
    (self: super: {
      zsh-git-prompt = super.callPackage ./zsh-git-prompt.nix { };
    })
  ];

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
        src = pkgs.zsh-git-prompt;
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
      ghci = "nix-shell -p ghc --run ghci";
    };
    initExtra = ''
      shell() {
        nix-shell --run "$*"
      }

      ${mkNixShellAliases {
        node = "nodejs";
        tree = "tree";
        zip = "zip";
        unzip = "unzip";
        file = "file";
      }}

      chpwd() {
        ${ls}
      }

      autoload -U colors
      colors
      ZSH_THEME_GIT_PROMPT_PREFIX=" ["
      ZSH_THEME_GIT_PROMPT_SHOW_UPSTREAM=1
      PROMPT='%F{cyan}%M %~%F{reset}$(git_super_status) %(?..%F{red}[%?]%F{reset} )$ '

      bindkey -v

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
