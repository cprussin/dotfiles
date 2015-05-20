{ lib, config, ... }:

{
  services.mingetty.helpLine = lib.mkForce "";
  services.mingetty.greetingLine = builtins.readFile (./. + "/${config.networking.hostName}");
}
