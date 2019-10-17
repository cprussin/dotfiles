{ config, ... }:

{
  imports = [
    ../base

    ../../modules/boot/logind
    ../../modules/boot/systemd-boot

    ../../modules/devices/android
    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/touchpad
    ../../modules/devices/virtualization

    ../../modules/devtools/docker

    ../../modules/net/wifi

    ../../modules/netflix/hostname
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/nix/plugins

    ../../modules/security/pass/system
    ../../modules/security/secure
    ../../modules/security/sshd

    ../../modules/ui/audio/system
    ../../modules/ui/fonts
    ../../modules/ui/log
    ../../modules/ui/opengl
    ../../modules/ui/setup-monitors/system
    ../../modules/ui/xorg

    ../../modules/zone/pacific
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

      ../../modules/ui/audio/user
      ../../modules/ui/autocutsel
      ../../modules/ui/backgrounds
      ../../modules/ui/backlight
      ../../modules/ui/dunst
      ../../modules/ui/emacs
      ../../modules/ui/gtk
      ../../modules/ui/launcher
      ../../modules/ui/minichrome
      ../../modules/ui/numix-cursor-theme
      ../../modules/ui/rofi-pass
      ../../modules/ui/screen-locker
      ../../modules/ui/setup-monitors/user
      ../../modules/ui/solarized-theme
      ../../modules/ui/terminal
      ../../modules/ui/tray
      ../../modules/ui/urxvt
      ../../modules/ui/xmobar
      ../../modules/ui/xmonad

      ../../modules/ui/theme/font/dejavu-sans-mono-11.nix
    ];

    home.packages = lib.mkForce [
      # FIXME: If numix-cursor-theme isn't in the environment, then the GTK
      # configuration won't be able to find it, since paths appear hardcoded in
      # GTK.  There's likely a way to pass the path to GTK apps instead.
      pkgs.numix-cursor-theme
    ];
  };
}
