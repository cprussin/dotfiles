{lib, ...}: {
  primary-user.home-manager = {
    services.gammastep = {
      enable = true;
      latitude = "37.24533400829229";
      longitude = "-121.84493315337309";
    };

    # sway's session rather than any graphical one: gammastep sets the gamma
    # through wlroots' gamma-control, which a domicile desk does not serve, so
    # there it would only fail and restart every few seconds.  home-manager
    # hardcodes `graphical-session.target` here rather than following
    # `wayland.systemd.target`.
    systemd.user.services.gammastep = {
      Unit = {
        After = lib.mkForce ["sway-session.target"];
        PartOf = lib.mkForce ["sway-session.target"];
      };
      Install.WantedBy = lib.mkForce ["sway-session.target"];
    };
  };
}
