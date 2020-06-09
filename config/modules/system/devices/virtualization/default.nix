{ ... }:

{
  virtualisation.libvirtd.enable = true;
  primary-user.extraGroups = [ "libvirtd" ];
  networking.firewall.checkReversePath = false;
}
