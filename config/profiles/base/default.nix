let
  sources = import ../../../sources.nix;
in
  _: {
    imports = [
      "${sources.home-manager}/nixos"
      ../../../modules

      ../../modules/data/backup
      ../../modules/data/session-vars
      ../../modules/data/syncthing
      ../../modules/data/xdg-user-dirs

      ../../modules/security/sshd
      ../../modules/security/ssl
      ../../modules/security/sudo
      ../../modules/security/umask

      ../../modules/system/nix
      ../../modules/system/stateVersion
      ../../modules/system/users

      ../../modules/ui/bash
      ../../modules/ui/dvp
      ../../modules/ui/fzf
      ../../modules/ui/greeting
      ../../modules/ui/htop
      ../../modules/ui/kitty
      ../../modules/ui/less
      ../../modules/ui/nushell
      ../../modules/ui/readline
      ../../modules/ui/terminfo
      ../../modules/ui/theme
      ../../modules/ui/zsh
    ];
  }
