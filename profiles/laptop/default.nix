{ config, ... }:

let
  nixosVersion = import ../../nixos-version.nix;
  home-manager = builtins.fetchTarball "https://github.com/rycee/home-manager/archive/release-${nixosVersion}.tar.gz";
in

{
  imports = [
    ../../modules/boot/logind
    ../../modules/boot/systemd-boot

    ../../modules/devices/android
    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/tmp
    ../../modules/devices/touchpad
    ../../modules/devices/virtualization

    ../../modules/devtools/docker

    ../../modules/net/dhcp
    ../../modules/net/wifi

    ../../modules/netflix/hostname
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/nix/plugins

    ../../modules/security/pass/system
    ../../modules/security/primary-user
    ../../modules/security/process-information-hiding
    ../../modules/security/secure
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

  home-manager.users.${config.primaryUserName} = { lib, pkgs, config, ... }: {
    imports = [
      ../../modules/data/email
      ../../modules/data/unison
      ../../modules/data/xdg-user-dirs

      ../../modules/devtools/git

      ../../modules/netflix/metatron
      ../../modules/netflix/newt

      ../../modules/nix/nixpkgs

      ../../modules/security/get-aws-access-key
      ../../modules/security/gpg
      ../../modules/security/pass/user
      ../../modules/security/secure
      ../../modules/security/ssh
      ../../modules/security/umask

      ../../modules/ui/audio/user
      ../../modules/ui/autocutsel
      ../../modules/ui/backgrounds
      ../../modules/ui/backlight
      ../../modules/ui/bash
      ../../modules/ui/dunst
      ../../modules/ui/dvp/user
      ../../modules/ui/emacs
      ../../modules/ui/gtk
      ../../modules/ui/launcher
      ../../modules/ui/minichrome
      ../../modules/ui/numix-cursor-theme
      ../../modules/ui/readline
      ../../modules/ui/rofi-pass
      ../../modules/ui/screen-locker
      ../../modules/ui/setup-monitors/user
      ../../modules/ui/solarized-theme
      ../../modules/ui/terminal
      ../../modules/ui/tray
      ../../modules/ui/urxvt
      ../../modules/ui/xmobar
      ../../modules/ui/xmonad
      ../../modules/ui/zsh
    ];

    home.packages = lib.mkForce [

      # FIXME: If numix-cursor-theme isn't in the environment, then the GTK
      # configuration won't be able to find it, since paths appear hardcoded
      # in GTK.  There's likely a way to pass the path to GTK apps instead.
      pkgs.numix-cursor-theme

      # FIXME: This is done under the hood in home-manager to set
      # sessionVariables.  We do still want this in the environment, even if
      # we want nothing else.  Ideally there should be a simpler way to clear
      # the environment except this file.
      (pkgs.writeTextFile {
        name = "hm-session-vars.sh";
        destination = "/etc/profile.d/hm-session-vars.sh";
        text = ''
            # Only source this once.
            if [ -n "$__HM_SESS_VARS_SOURCED" ]; then return; fi
            export __HM_SESS_VARS_SOURCED=1
            ${config.lib.shell.exportAll config.home.sessionVariables}
          '';
      })

    ];
  };
}
