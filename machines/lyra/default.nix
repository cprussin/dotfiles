{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primaryUserName = "cprussin";
  networking.hostName = "lyra";
}
