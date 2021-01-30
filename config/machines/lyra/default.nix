{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "lyra";
    hostId = "a3e3c46d";
  };
  environment.etc."machine-id".text = "03f4ed7a942748e6bd5591367c99d2cc\n";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
