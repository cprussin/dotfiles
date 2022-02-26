{pkgs, ...}: {
  services.printing = {
    enable = true;
    drivers = [pkgs.epson-escpr2];
  };
  hardware.printers = {
    ensureDefaultPrinter = "circinus";
    ensurePrinters = [
      {
        name = "circinus";
        location = "Office";
        description = "Epson ET-3760";
        deviceUri = "https://circinus:631/ipp/print";
        model = "epson-inkjet-printer-escpr2/Epson-ET-3760_Series-epson-escpr2-en.ppd";
      }
    ];
  };
}
