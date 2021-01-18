{ pkgs, ... }:

{
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
  };
  hardware.printers = {
    ensureDefaultPrinter = "circinus";
    ensurePrinters = [
      {
        name = "circinus";
        location = "Office";
        description = "HP Photosmart 7520";
        deviceUri = "hp:/net/Photosmart_7520_series?hostname=circinus";
        model = "drv:///hp/hpcups.drv/hp-photosmart_7520_series.ppd";
      }
    ];
  };
}
