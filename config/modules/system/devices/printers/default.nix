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

  # `-m everywhere` fetches the queue's capabilities over IPP, which only works
  # once the wireguard tunnel carries traffic -- and on a laptop that's a while
  # after the tunnel's units finish, so retry too.
  systemd.services.ensure-printers = {
    wants = ["wireguard-prussinnet.target"];
    after = ["wireguard-prussinnet.target"];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "15s";
    };

    # Caps total attempts rather than attempts per window: a failure against the
    # tunnel's black-holed nameservers takes long enough to reset a finite
    # window instead of tripping it.  Not `TimeoutStartSec` -- `-m everywhere`
    # is fetched by cupsd in a thread that outlives a killed lpadmin, and a
    # stale one failing after a later attempt succeeded marks the new queue
    # temporary, so cupsd deletes it.  Hitting the cap means a start by hand
    # needs `systemctl reset-failed` first.
    unitConfig = {
      StartLimitIntervalSec = "infinity";
      StartLimitBurst = 20;
    };
  };
}
