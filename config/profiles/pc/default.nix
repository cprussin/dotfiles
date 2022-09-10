{...}: {
  imports = [
    ../physical-machine

    ../../modules/data/git

    ../../modules/security/gpg
    ../../modules/security/pass
    ../../modules/security/secure
    ../../modules/security/ssh
    ../../modules/security/wireguard

    ../../modules/system/avahi
    ../../modules/system/dbus
    ../../modules/system/devices/android
    ../../modules/system/devices/audio
    ../../modules/system/devices/cdrom
    ../../modules/system/devices/dialout
    ../../modules/system/devices/printers
    ../../modules/system/devices/redshift
    ../../modules/system/devices/scanners

    ../../modules/ui/audio
    ../../modules/ui/direnv
    ../../modules/ui/emacs
    ../../modules/ui/fonts
    ../../modules/ui/gtk
    ../../modules/ui/imv
    ../../modules/ui/launcher
    ../../modules/ui/log
    ../../modules/ui/lorri
    ../../modules/ui/mako
    ../../modules/ui/mpv
    ../../modules/ui/opengl
    ../../modules/ui/sway
    ../../modules/ui/swayidle
    ../../modules/ui/swaylock
    ../../modules/ui/waybar
  ];

  enableSshdAtBoot = false;
  persistSyncthingKeys = true;
}
