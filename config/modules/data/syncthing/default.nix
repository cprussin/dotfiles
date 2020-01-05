{ ... }:

{
  primary-user.home-manager.services.syncthing.enable = true;

  networking.firewall = {
    allowedTCPPorts = [ 22000 ];
    allowedUDPPorts = [ 21027 ];
  };
}
