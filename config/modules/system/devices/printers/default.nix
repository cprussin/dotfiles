_: {
  services.printing.enable = true;
  hardware.printers = {
    ensureDefaultPrinter = "circinus";
    ensurePrinters = [
      {
        name = "circinus";
        location = "Office";
        description = "Epson ET-3760";
        deviceUri = "ipp://circinus.internal.prussin.net/printers/circinus";
        model = "everywhere";
      }
    ];
  };
}
