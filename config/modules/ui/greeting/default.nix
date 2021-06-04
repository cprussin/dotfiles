{ lib, ... }:

{
  services.getty.helpLine = lib.mkForce "";
}
