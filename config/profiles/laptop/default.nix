{ ... }:

{
  imports = [
    ../base

    ../../modules/boot/logind
    ../../modules/boot/systemd-boot

    ../../modules/data/email
    ../../modules/data/syncthing
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

    ../../modules/nix/nix-path

    ../../modules/security/get-aws-access-key
    ../../modules/security/gpg
    ../../modules/security/pass
    ../../modules/security/ssh

    ../../modules/system/dbus

    ../../modules/ui/audio
    ../../modules/ui/direnv
    ../../modules/ui/emacs
    ../../modules/ui/gtk
    ../../modules/ui/icon-fonts
    ../../modules/ui/imv
    ../../modules/ui/launcher
    ../../modules/ui/log
    ../../modules/ui/lorri
    ../../modules/ui/mako
    ../../modules/ui/mpv
    ../../modules/ui/opengl
    ../../modules/ui/sway
    ../../modules/ui/swaylock
    ../../modules/ui/theme
    ../../modules/ui/waybar
    ../../modules/ui/zathura

    ../../modules/zone/pacific
  ];

  promptColor = "cyan";
  enableTermiteApplicationConfig = true;
  enableSshdAtBoot = false;
  services.openssh.permitRootLogin = "no";
  primary-user.secure.enableMount = true;
}
