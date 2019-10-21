{ ... }:

let
  nixosVersion = import ../../nixos-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${nixosVersion}.tar.gz";
in

{
  imports = [
    "${home-manager}/nixos"

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
    ../../modules/ui/dvp
    ../../modules/ui/greeting
    ../../modules/ui/readline
    ../../modules/ui/zsh
  ];
}
