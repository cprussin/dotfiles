{ ... }:
let
  sources = import ../../../sources.nix;
in
{
  imports = [
    "${sources.home-manager}/nixos"
    ../../../modules

    ../../modules/data/session-vars
    ../../modules/data/xdg-user-dirs

    ../../modules/security/process-information-hiding
    ../../modules/security/sshd
    ../../modules/security/sudo
    ../../modules/security/umask

    ../../modules/system/nix-binary-caches
    ../../modules/system/nixpkgs
    ../../modules/system/users

    ../../modules/ui/bash
    ../../modules/ui/dvp
    ../../modules/ui/fzf
    ../../modules/ui/greeting
    ../../modules/ui/htop
    ../../modules/ui/less
    ../../modules/ui/readline
    ../../modules/ui/termite
    ../../modules/ui/terminfo
    ../../modules/ui/theme
    ../../modules/ui/zsh
  ];
}
