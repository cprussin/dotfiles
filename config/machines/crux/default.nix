{ ... }:

{
  imports = [
    ./hardware.nix
    ./backup.nix
    ./plex.nix
    ./dynamic-dns.nix
    ../../profiles/physical-machine
    ../../profiles/server
    ../../modules/data/syncthing
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "crux";
    hostId = "a362c6ea";
  };
  services.mingetty.greetingLine = builtins.readFile ./greeting;

  programs.powerpanel.enable = true;
}
