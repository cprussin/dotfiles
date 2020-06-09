{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
    ../../modules/system/devices/wifi
  ];

  primary-user.name = "cprussin";
  networking.hostName = "orion";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
