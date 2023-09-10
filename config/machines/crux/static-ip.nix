{
  config,
  pkgs,
  ...
}: let
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  networking = {
    interfaces."${config.interfaces.eth}".ipv4.addresses = [
      {
        inherit (network.home.static.crux) address;
        inherit (network.home) prefixLength;
      }
    ];
    defaultGateway = network.home.net-hardware.router.address;
  };
}
