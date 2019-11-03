{ lib, ... }:

{
  services.mingetty.helpLine = lib.mkForce "";
}
