{ config, ... }:

{
  virtualisation.libvirtd.enable = true;
  users.users.${config.primaryUserName}.extraGroups = [ "libvirtd" ];
  networking.firewall.checkReversePath = false;
}
