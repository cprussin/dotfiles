{config, ...}: let
  # Sunshine's ports are offsets from its base port.
  mkPorts = map (offset: config.services.sunshine.settings.port + offset);
in {
  services.sunshine = {
    enable = true;

    # Sunshine captures the screen and injects input events, so it does not run
    # for the whole session: the `cast` launcher app starts and stops it.
    autoStart = false;

    # KMS capture reads planes that belong to another DRM master, which needs
    # CAP_SYS_ADMIN; this grants it through a setcap wrapper.
    capSysAdmin = true;

    settings = {
      # sway is wlroots, but Sunshine's wlr-screencopy backend fails there:
      # frame capture errors out on EGL image creation and the stream comes
      # through as black with horizontal lines (LizardByte/Sunshine#5258).
      # Grab the framebuffer from DRM/KMS instead.
      #
      # `output_name` is deliberately unset so Sunshine streams the default
      # display: which output that should be depends on the kanshi profile in
      # effect, and there is no stable answer to bake in here.
      capture = "kms";

      # The web UI answers any LAN host by default; restrict it to local
      # requests.  The `sunshine` launcher app opens it from this machine,
      # which is the only place it is ever used from.  Note this rejects
      # remote requests rather than changing what Sunshine binds: it still
      # listens on every address, so the port stays closed below too.
      origin_web_ui_allowed = "pc";
    };
  };

  # This is `services.sunshine.openFirewall`'s own list with the web UI (offset
  # +1) taken out, and it is written out here so that dropping it back in is
  # visible in review.  That port is the one endpoint `origin_web_ui_allowed`
  # does not cover: `savePassword` skips `authenticate` outright while no admin
  # password is set, and the origin check lives inside `authenticate`, so
  # between the first `cast` and finishing setup anything that can reach it can
  # claim the account.  Everything else on these ports is either harmless to
  # answer (`/serverinfo`, and `/pair`, which is gated on the PIN) or needs a
  # paired client certificate.
  #
  # These are open on every interface, unlike the interface-scoped rules
  # elsewhere in this repo: the laptop casts from wifi or the dock's ethernet
  # depending on the day, and this also puts them on the tailnet.
  networking.firewall = {
    # -5 nvhttp HTTPS, 0 nvhttp HTTP, 21 RTSP.  (+1 is the web UI.)
    allowedTCPPorts = mkPorts [(-5) 0 21];

    # 9 video, 10 control, 11 audio, 13 mic, 21 RTSP.
    allowedUDPPorts = mkPorts [9 10 11 13 21];
  };

  # Enabling Sunshine turns on Avahi publishing: the upstream module sets
  # `services.avahi.publish.enable` and `.userServices`, and since nothing here
  # defines either, its `mkDefault` takes effect.  That is what lets Moonlight
  # discover the laptop by name, but it is system-wide and not tied to Sunshine
  # running: lyra advertises its hostname and addresses on every network it
  # joins from here on.  Setting `publish.enable = false` in the avahi module
  # would override it, at the cost of adding the laptop to Moonlight by IP.
}
