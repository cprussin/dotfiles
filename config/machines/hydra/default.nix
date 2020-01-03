{ ... }:

{
  imports = [
    ../../profiles/cloud
  ];

  primary-user.name = "cprussin";
  networking.firewall.allowedTCPPorts = [];
}
