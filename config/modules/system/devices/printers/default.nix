{pkgs, ...}: let
  network = pkgs.callPackage ../../../../../lib/network.nix {};
in {
  services.printing.enable = true;
  hardware.printers = {
    ensureDefaultPrinter = "circinus";
    ensurePrinters = [
      {
        name = "circinus";
        location = "Office";
        description = "Epson ET-3760";
        deviceUri = "ipp://[${network.wireguard.crux.address}]:631/printers/circinus";
        model = "everywhere";
      }
    ];
  };
}
