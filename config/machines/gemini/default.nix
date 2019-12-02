{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking.hostName = "gemini";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
