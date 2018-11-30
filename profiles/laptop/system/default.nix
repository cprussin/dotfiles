{ hostName, userName }: { lib, ... }:

{
  imports = [
    ../../../modules/auth/primary-user
    ../../../modules/auth/sudo

    ../../../modules/boot/systemd-boot

    (../../../modules/hardware + "/${hostName}")

    ../../../modules/misc/auto-upgrade
    ../../../modules/misc/docker
    ../../../modules/misc/printing

    ../../../modules/netflix/hostname
    ../../../modules/netflix/shakti-nginx
    ../../../modules/netflix/vpn

    ../../../modules/networks/bluetooth
    ../../../modules/networks/dhcp
    ../../../modules/networks/wifi

    ../../../modules/timezone/pacific

    ../../../modules/ui/audio
    ../../../modules/ui/dvp-system
    ../../../modules/ui/fonts
    ../../../modules/ui/greeting
    ../../../modules/ui/log
    ../../../modules/ui/touchpad
    ../../../modules/ui/xorg
  ];

  primaryUserName = userName;
  networking.hostName = hostName;
  system.stateVersion = import ../../../state-version.nix;
}
