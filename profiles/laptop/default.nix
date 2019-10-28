{ pkgs, ... }:

let
  themeUtil = pkgs.callPackage ../../lib/themes.nix {};
in

{
  imports = [
    ../base

    ../../modules/boot/logind
    ../../modules/boot/systemd-boot

    ../../modules/data/email
    ../../modules/data/unison
    ../../modules/data/xdg-user-dirs

    ../../modules/devices/android
    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/touchpad
    ../../modules/devices/virtualization

    ../../modules/devtools/docker
    ../../modules/devtools/git

    ../../modules/net/wifi

    ../../modules/netflix/ca
    ../../modules/netflix/hostname
    ../../modules/netflix/metatron
    ../../modules/netflix/newt
    ../../modules/netflix/shakti-nginx
    ../../modules/netflix/vpn

    ../../modules/security/get-aws-access-key
    ../../modules/security/gpg
    ../../modules/security/pass
    ../../modules/security/secure
    ../../modules/security/ssh
    ../../modules/security/sshd

    ../../modules/ui/autocutsel
    ../../modules/ui/audio
    ../../modules/ui/backgrounds
    ../../modules/ui/backlight
    ../../modules/ui/dunst
    ../../modules/ui/emacs
    ../../modules/ui/fonts
    ../../modules/ui/gtk
    ../../modules/ui/launcher
    ../../modules/ui/log
    ../../modules/ui/minichrome
    ../../modules/ui/numix-cursor-theme
    ../../modules/ui/opengl
    ../../modules/ui/rofi-pass
    ../../modules/ui/screen-locker
    ../../modules/ui/setup-monitors
    ../../modules/ui/solarized-theme
    ../../modules/ui/terminal
    ../../modules/ui/theme/cursor
    ../../modules/ui/theme/font
    ../../modules/ui/theme/icon
    ../../modules/ui/tray
    ../../modules/ui/urxvt
    ../../modules/ui/xmobar
    ../../modules/ui/xmonad
    ../../modules/ui/xorg

    ../../modules/zone/pacific
  ];

  fontTheme = themeUtil.font "dejavu-sans-mono-11";
  iconTheme = themeUtil.icon "papirus-48";
  cursorTheme = themeUtil.cursor "numix";
}
