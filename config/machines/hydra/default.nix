{ ... }:

{
  imports = [
    ../../profiles/server
  ];

  primary-user.name = "cprussin";
  networking.firewall.allowedTCPPorts = [];
}
