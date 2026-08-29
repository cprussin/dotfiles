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

  systemd = {
    # `-m everywhere` fetches the queue's capabilities over IPP, which only
    # works once the wireguard tunnel carries traffic.  Don't bound an attempt
    # with `TimeoutStartSec`: cupsd does that fetch in a thread that outlives a
    # killed lpadmin, and a stale one failing after a later attempt succeeded
    # marks the queue it created temporary, so cupsd deletes it.
    services.ensure-printers = {
      wants = ["wireguard-prussinnet.target"];
      after = ["wireguard-prussinnet.target"];
    };

    # Ordering isn't enough on a laptop: the tunnel's units finish well before
    # wifi has associated and the handshake has landed, so the boot run usually
    # fails.  `RemainAfterExit` leaves the unit active once a run succeeds, so
    # this only fires while it's still failing -- which also covers coming back
    # onto the tunnel long after boot.
    timers.ensure-printers = {
      wantedBy = ["timers.target"];
      timerConfig.OnUnitInactiveSec = "2min";
    };
  };
}
