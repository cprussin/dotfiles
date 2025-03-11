{...}: {
  imports = [
    ../physical-machine

    ../../modules/data/git
    ../../modules/data/library

    ../../modules/security/gpg
    ../../modules/security/pass
    ../../modules/security/ssh
    ../../modules/security/tailscale
    ../../modules/security/wireguard
    ../../modules/security/yubikey

    ../../modules/system/avahi
    ../../modules/system/dbus
    ../../modules/system/devices/android
    ../../modules/system/devices/audio
    ../../modules/system/devices/cdrom
    ../../modules/system/devices/dialout
    ../../modules/system/devices/ledger
    ../../modules/system/devices/printers
    ../../modules/system/devices/redshift
    ../../modules/system/devices/scanners
    ../../modules/system/podman

    ../../modules/ui/direnv
    ../../modules/ui/emacs
    ../../modules/ui/fonts
    ../../modules/ui/graphics
    ../../modules/ui/gtk
    ../../modules/ui/imv
    ../../modules/ui/launcher
    ../../modules/ui/log
    ../../modules/ui/lorri
    ../../modules/ui/mako
    ../../modules/ui/mpv
    ../../modules/ui/plymouth
    ../../modules/ui/sway
    ../../modules/ui/swayidle
    ../../modules/ui/swaylock
    ../../modules/ui/waybar
    ../../modules/ui/xdg-portal
  ];

  enableSshdAtBoot = false;
  persistSyncthingKeys = true;
}
