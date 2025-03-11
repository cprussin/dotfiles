{...}: {
  imports = [
    ../../../modules

    ../../modules/data/backup
    ../../modules/data/session-vars
    ../../modules/data/syncthing
    ../../modules/data/xdg-user-dirs

    ../../modules/security/sshd
    ../../modules/security/ssl
    ../../modules/security/sudo
    ../../modules/security/umask

    ../../modules/system/bpftune
    ../../modules/system/nix
    ../../modules/system/stateVersion
    ../../modules/system/users

    ../../modules/ui/bash
    ../../modules/ui/btop
    ../../modules/ui/dvp
    ../../modules/ui/fzf
    ../../modules/ui/greeting
    ../../modules/ui/kitty
    ../../modules/ui/less
    ../../modules/ui/nushell
    ../../modules/ui/readline
    ../../modules/ui/starship
    ../../modules/ui/terminfo
    ../../modules/ui/theme
  ];
}
