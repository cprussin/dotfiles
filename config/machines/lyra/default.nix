{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking.hostName = "lyra";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
