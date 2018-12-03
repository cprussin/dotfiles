{ lib, ... }:

{
  systemd.services.dhcpcd.before = lib.mkForce [ "network-online.target" ];
}
