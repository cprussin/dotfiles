{ config, ... }:

let
  state-version = import ../../state-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${state-version}.tar.gz";
in

{
  imports = [
    ../../modules/boot/systemd-boot

    ../../modules/devices/bluetooth
    ../../modules/devices/printing
    ../../modules/devices/touchpad

    ../../modules/devtools/docker

    ../../modules/net/dhcp
    ../../modules/net/wifi

    ../../modules/netflix/hostname
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/nix/auto-upgrade

    ../../modules/security/primary-user
    ../../modules/security/sshd
    ../../modules/security/sudo

    ../../modules/ui/audio/system
    ../../modules/ui/dvp/system
    ../../modules/ui/fonts
    ../../modules/ui/greeting
    ../../modules/ui/log
    ../../modules/ui/opengl
    ../../modules/ui/xorg

    ../../modules/zone/pacific

    "${home-manager}/nixos"
  ];

  home-manager.users.${config.primaryUserName} = { ... }: {
    imports = [
      ../../modules/data/email
      ../../modules/data/nogit
      ../../modules/data/unison
      ../../modules/data/xdg-user-dirs

      ../../modules/devtools/git

      ../../modules/netflix/metatron

      ../../modules/nix/nixpkgs

      ../../modules/security/gpg
      ../../modules/security/keepassxc
      ../../modules/security/ssh
      ../../modules/security/umask

      ../../modules/ui/audio/user
      ../../modules/ui/autocutsel
      ../../modules/ui/backgrounds
      ../../modules/ui/dvp/user
      ../../modules/ui/emacs
      ../../modules/ui/launcher
      ../../modules/ui/numix-cursor-theme
      ../../modules/ui/readline
      ../../modules/ui/setup-monitors
      ../../modules/ui/solarized-theme
      ../../modules/ui/terminal
      ../../modules/ui/tray
      ../../modules/ui/urxvt
      ../../modules/ui/xmobar
      ../../modules/ui/xmonad
      ../../modules/ui/zsh
    ];

    home.stateVersion = state-version;
  };

  system.stateVersion = state-version;
}
