{ ... }:

{
  imports = [
    ./hardware.nix
    ../../profiles/pc
    ../../modules/system/devices/wifi
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "orion";
    hostId = "4dcafc70";
  };
  environment.etc."machine-id".text = "3c4b8d0c95ec4008bfe76f943bc2fa17\n";
  services.mingetty.greetingLine = builtins.readFile ./greeting;
}
