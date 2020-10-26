{ ... }:

{
  imports = [
    ./hardware.nix
    ./backup.nix
    ./plex.nix
    ./dynamic-dns.nix
    ./acme.nix
    ../../profiles/physical-machine
    ../../profiles/server
    ../../modules/data/syncthing
    ../../modules/ui/home-assistant
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  services = {
    mingetty.greetingLine = builtins.readFile ./greeting;
    home-assistant.virtualHost = "720-natoma-drive.prussin.net";
  };

  programs.powerpanel.enable = true;
}
