{ ... }:

{
  imports = [
    ./hardware.nix
    ./backp.nix
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
}
