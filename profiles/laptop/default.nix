{ config, ... }:

let
  stateVersion = import ../../state-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${stateVersion}.tar.gz";
in

{
  imports = [
    ../../modules/boot/systemd-boot

    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/touchpad

    ../../modules/devtools/docker

    ../../modules/net/dhcp
    ../../modules/net/wifi

    ../../modules/netflix/hostname
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/nix/auto-upgrade

    ../../modules/security/primary-user
    ../../modules/security/process-information-hiding
    ../../modules/security/sshd
    ../../modules/security/sudo

    ../../modules/ui/audio/system
    ../../modules/ui/dvp/system
    ../../modules/ui/fonts
    ../../modules/ui/greeting
    ../../modules/ui/log
    ../../modules/ui/opengl
    ../../modules/ui/setup-monitors/system
    ../../modules/ui/xorg

    ../../modules/zone/pacific

    "${home-manager}/nixos"
  ];

  home-manager.users.${config.primaryUserName} = { lib, pkgs, ... }: {
    imports = [
      ../../modules/data/email
      ../../modules/data/nogit
      ../../modules/data/unison
      ../../modules/data/xdg-user-dirs

      ../../modules/devtools/git

      ../../modules/netflix/metatron

      ../../modules/nix/nixpkgs

      ../../modules/security/gpg
      ../../modules/security/ssh
      ../../modules/security/umask

      ../../modules/ui/audio/user
      ../../modules/ui/autocutsel
      ../../modules/ui/backgrounds
      ../../modules/ui/backlight
      ../../modules/ui/dunst
      ../../modules/ui/dvp/user
      ../../modules/ui/emacs
      ../../modules/ui/gtk
      ../../modules/ui/launcher
      ../../modules/ui/minichrome
      ../../modules/ui/numix-cursor-theme
      ../../modules/ui/readline
      ../../modules/ui/rofi-pass
      ../../modules/ui/setup-monitors/user
      ../../modules/ui/solarized-theme
      ../../modules/ui/terminal
      ../../modules/ui/tray
      ../../modules/ui/urxvt
      ../../modules/ui/xmobar
      ../../modules/ui/xmonad
      ../../modules/ui/zsh
    ];

    home = {
      inherit stateVersion;

      packages = lib.mkForce [

        # FIXME: If numix-cursor-theme isn't in the environment, then the GTK
        # configuration won't be able to find it, since paths appear hardcoded
        # in GTK.  There's likely a way to pass the path to GTK apps instead.
        pkgs.numix-cursor-theme

      ];
    };
  };

  system = { inherit stateVersion; };
}
