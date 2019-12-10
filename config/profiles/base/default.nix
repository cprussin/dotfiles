{ ... }:

{
  imports = [
    <home-manager/nixos>
    ../../../modules

    ../../modules/data/session-vars

    ../../modules/devices/tmp

    ../../modules/nix/nix-path
    ../../modules/nix/nixpkgs
    ../../modules/nix/plugins

    ../../modules/security/process-information-hiding
    ../../modules/security/sudo
    ../../modules/security/umask

    ../../modules/ui/bash
    ../../modules/ui/direnv
    ../../modules/ui/dvp
    ../../modules/ui/greeting
    ../../modules/ui/lorri
    ../../modules/ui/readline
    ../../modules/ui/zsh
  ];
}
