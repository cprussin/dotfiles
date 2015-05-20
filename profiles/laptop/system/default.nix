{ hostName, userName }: { lib, ... }:

{
  imports = [
    ../../../modules/auth/primary-user
    ../../../modules/auth/sudo

    ../../../modules/boot/systemd-boot

    (../../../modules/hardware + "/${hostName}")

    ../../../modules/misc/docker
    ../../../modules/misc/printing

    ../../../modules/netflix/hostname
    ../../../modules/netflix/shakti-nginx
    ../../../modules/netflix/vpn

    ../../../modules/networks/wifi
    ../../../modules/networks/bluetooth

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
  systemd.services.dhcpcd.before = lib.mkForce [ "network-online.target" ];
  system.autoUpgrade.enable = true;
  system.stateVersion = "18.09";
}
