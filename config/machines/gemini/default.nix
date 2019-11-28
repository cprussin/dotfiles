{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primaryUserName = "cprussin";
  networking.hostName = "gemini";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
