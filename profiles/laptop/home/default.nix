{ pkgs, lib, config, ... }:

{
  imports = [
    ../../../modules/auth/gpg
    ../../../modules/auth/ssh
    ../../../modules/auth/umask

    ../../../modules/data/email
    ../../../modules/data/git
    ../../../modules/data/unison
    ../../../modules/data/xdg-user-dirs

    ../../../modules/misc/user-bin

    ../../../modules/netflix/metatron

    ../../../modules/nix/home-manager
    ../../../modules/nix/nixlocal
    ../../../modules/nix/nixpkgs

    ../../../modules/nogit

    ../../../modules/ui/autocutsel
    ../../../modules/ui/backgrounds
    ../../../modules/ui/dvp-user
    ../../../modules/ui/emacs
    ../../../modules/ui/numix-cursor-theme
    ../../../modules/ui/readline
    ../../../modules/ui/solarized-theme
    ../../../modules/ui/tray
    ../../../modules/ui/urxvt
    ../../../modules/ui/xmobar
    ../../../modules/ui/xmonad
    ../../../modules/ui/zsh
  ];

  home.stateVersion = import ../../../state-version.nix;
}
