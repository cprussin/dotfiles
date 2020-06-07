{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking.hostName = "orion";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
