{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "lyra";
    hostId = "72b0fa88";
  };
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
