{pkgs, ...}: let
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  services.printing = {
    enable = true;
    stateless = true;
    startWhenNeeded = false;
    drivers = [pkgs.epson-escpr2];
    defaultShared = true;
    listenAddresses = ["[${network.wireguard.crux.address}]:631"];
    allowFrom = ["@IF(prussinnet)" "localhost"];
  };
  hardware.printers = {
    ensureDefaultPrinter = "circinus";
    ensurePrinters = [
      {
        name = "circinus";
        location = "Office";
        description = "Epson ET-3760";
        deviceUri = "https://${network.home.circinus.address}:631/ipp/print";
        model = "epson-inkjet-printer-escpr2/Epson-ET-3760_Series-epson-escpr2-en.ppd";
      }
    ];
  };
  networking.firewall.interfaces.prussinnet = {
    allowedTCPPorts = [631];
    allowedUDPPorts = [631];
  };
}
