{ ... }:

{
  imports = [
    ../physical-machine

    ../../modules/data/email
    ../../modules/data/git
    ../../modules/data/syncthing
    ../../modules/data/xdg-user-dirs

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

    ../../modules/system/dbus
    ../../modules/system/devices/android
    ../../modules/system/devices/printers
    ../../modules/system/devices/virtualization
    ../../modules/system/docker
    ../../modules/system/inotify-max-user-watches

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
    ../../modules/ui/waybar
    ../../modules/ui/zathura
  ];

  promptColor = "cyan";
  enableTermiteApplicationConfig = true;
  enableSshdAtBoot = false;
  services.openssh.permitRootLogin = "no";
}
