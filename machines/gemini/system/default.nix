{ ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/laptop/system
  ];

  primaryUserName = "cprussin";
  networking.hostName = "gemini";
}
