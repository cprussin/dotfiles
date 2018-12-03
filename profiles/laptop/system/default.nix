{ lib, ... }:

{
  imports = [
    ../../../modules/boot/systemd-boot

    ../../../modules/devices/bluetooth
    ../../../modules/devices/printing
    ../../../modules/devices/touchpad

    ../../../modules/devtools/docker

    ../../../modules/net/dhcp
    ../../../modules/net/wifi

    ../../../modules/netflix/hostname
    ../../../modules/netflix/shakti-nginx
    ../../../modules/netflix/vpn

    ../../../modules/nix/auto-upgrade

    ../../../modules/security/primary-user
    ../../../modules/security/sudo

    ../../../modules/ui/audio
    ../../../modules/ui/dvp/system
    ../../../modules/ui/fonts
    ../../../modules/ui/greeting
    ../../../modules/ui/log
    ../../../modules/ui/xorg

    ../../../modules/zone/pacific
  ];

  system.stateVersion = import ../../../state-version.nix;
}
