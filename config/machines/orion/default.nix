{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
    ../../modules/system/devices/wifi
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "orion";
    hostId = "4dcafc70";
  };
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
