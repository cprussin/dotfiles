{ ... }:

let
  sources = import ../../../sources.nix;
in

{
  imports = [
    "${sources.home-manager}/nixos"
    ../../../modules

    ../../modules/data/session-vars

    ../../modules/security/process-information-hiding
    ../../modules/security/sshd
    ../../modules/security/sudo
    ../../modules/security/umask

    ../../modules/system/nix-binary-caches
    ../../modules/system/nix-path
    ../../modules/system/nixpkgs

    ../../modules/ui/bash
    ../../modules/ui/dvp
    ../../modules/ui/fzf
    ../../modules/ui/greeting
    ../../modules/ui/readline
    ../../modules/ui/termite
    ../../modules/ui/terminfo
    ../../modules/ui/zsh
  ];
}
