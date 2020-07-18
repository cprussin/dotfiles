{ ... }:

{
  primary-user.home-manager.services.syncthing.enable = true;

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

  networking.firewall = {
    allowedTCPPorts = [ 22000 ];
    allowedUDPPorts = [ 21027 ];
  };
}
