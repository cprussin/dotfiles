{ ... }:

{
  imports = [
    ../base

    ../../modules/boot/logind
    ../../modules/boot/systemd-boot

    ../../modules/data/email
    ../../modules/data/xdg-user-dirs

    ../../modules/devices/android
    ../../modules/devices/backlight
    ../../modules/devices/bluetooth
    ../../modules/devices/printers
    ../../modules/devices/touchpad
    ../../modules/devices/virtualization

    ../../modules/devtools/git
    ../../modules/devtools/inotify-max-user-watches

    ../../modules/net/wifi

    ../../modules/netflix/ca
    ../../modules/netflix/hostname
    ../../modules/netflix/metatron
    ../../modules/netflix/newt
    ../../modules/netflix/vpn

    ../../modules/security/get-aws-access-key
    ../../modules/security/gpg
    ../../modules/security/pass
    ../../modules/security/secure
    ../../modules/security/ssh
    ../../modules/security/sshd

    ../../modules/ui/audio
    ../../modules/ui/emacs
    ../../modules/ui/gtk
    ../../modules/ui/icon-fonts
    ../../modules/ui/imv
    ../../modules/ui/launcher
    ../../modules/ui/log
    ../../modules/ui/mako
    ../../modules/ui/mpv
    ../../modules/ui/opengl
    ../../modules/ui/sway
    ../../modules/ui/swaylock
    ../../modules/ui/termite
    ../../modules/ui/theme
    ../../modules/ui/waybar
    ../../modules/ui/zathura

    ../../modules/zone/pacific
  ];
}
