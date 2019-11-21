{ ... }:

{
  imports = [
    <home-manager/nixos>

    ../../modules/data/session-vars

    ../../modules/devices/tmp

    ../../modules/nix/nixpkgs
    ../../modules/nix/plugins
    ../../modules/nix/trusted-users

    ../../modules/security/primary-user
    ../../modules/security/process-information-hiding
    ../../modules/security/sudo
    ../../modules/security/umask

    ../../modules/ui/bash
    ../../modules/ui/direnv
    ../../modules/ui/dvp
    ../../modules/ui/greeting
    ../../modules/ui/readline
    ../../modules/ui/zsh
  ];
}
