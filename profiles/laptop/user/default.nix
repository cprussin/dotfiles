{ pkgs, lib, config, ... }:

{
  imports = [
    ../../../modules/data/email
    ../../../modules/data/nogit
    ../../../modules/data/unison
    ../../../modules/data/xdg-user-dirs

    ../../../modules/devtools/git

    ../../../modules/netflix/metatron

    ../../../modules/nix/home-manager
    ../../../modules/nix/nixpkgs

    ../../../modules/security/gpg
    ../../../modules/security/keepassxc
    ../../../modules/security/ssh
    ../../../modules/security/umask

    ../../../modules/ui/autocutsel
    ../../../modules/ui/backgrounds
    ../../../modules/ui/dvp/user
    ../../../modules/ui/emacs
    ../../../modules/ui/numix-cursor-theme
    ../../../modules/ui/readline
    ../../../modules/ui/solarized-theme
    ../../../modules/ui/tray
    ../../../modules/ui/urxvt
    ../../../modules/ui/user-bin
    ../../../modules/ui/xmobar
    ../../../modules/ui/xmonad
    ../../../modules/ui/zsh
  ];

  home.stateVersion = import ../../../state-version.nix;
}
