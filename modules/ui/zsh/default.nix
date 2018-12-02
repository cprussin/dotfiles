{ pkgs, ... }:

let
  ls = pkgs.callPackage ./ls.nix { };
  clear = pkgs.callPackage ./clear.nix { inherit ls; };
in

{
  home = {
    packages = [ pkgs.python ]; # For gitstatus
    file.".zsh-dircolors.config".text = "dircolors.ansi-dark";
  };

  programs.zsh = {
    enable = true;
    plugins = [
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.6.0";
          sha256 = "0zmq66dzasmr5pwribyh4kbkk23jxbpdw4rjxx0i7dx8jjp2lzl4";
        };
      }
      {
        name = "zsh-git-prompt";
        file = "zshrc.sh";
        src = pkgs.fetchFromGitHub {
          owner = "starcraftman";
          repo = "zsh-git-prompt";
          rev = "11b83ba3b85d14c66cf2ab79faefab6d838da28e";
          sha256 = "04aylsjfb03ckw219plkzpyiq4j9g66bjxa5pa56h1p7df6pjssb";
        };
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
    };
    initExtra = ''
      function chpwd() {
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
