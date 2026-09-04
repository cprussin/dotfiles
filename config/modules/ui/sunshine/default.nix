# See docs/casting-design.org for the reasoning behind the choices here.
{
  config,
  lib,
  pkgs,
  ...
}: let
  # Sunshine's ports are offsets from its base port.
  mkPorts = map (offset: config.services.sunshine.settings.port + offset);

  # Same generator and input as the upstream module's own config file, which it
  # does not expose but the wrapper below has to name.
  configFile = (pkgs.formats.keyValue {}).generate "sunshine.conf" config.services.sunshine.settings;

  # `cast` names the output it just created, which is not knowable from the
  # store.  Sunshine's `name=value` arguments override the config file.
  sunshine-cast = pkgs.writeShellScript "sunshine-cast" ''
    output="$(${pkgs.coreutils}/bin/cat "$XDG_RUNTIME_DIR/cast-output" 2>/dev/null)"

    # A bare `output_name=` is a malformed argument, not an empty setting:
    # Sunshine prints its help and returns 0, which systemd calls a clean start.
    if ${pkgs.coreutils}/bin/test -z "$output"
    then
      echo 'sunshine: no cast output recorded -- start it with the `cast` launcher app' >&2
      exit 1
    fi

    # Blanking takes the headless output down and nothing about a cast counts
    # as activity -- Moonlight sends the Fire TV remote as a gamepad, which
    # libinput ignores.  swayidle drops *both* its timeouts while logind
    # reports an idle inhibitor -- the idle auto-lock as well as blanking --
    # but keeps running, so the `lock` and `before-sleep` handlers still fire.
    # Holding it here ties it to the process, so no exit path leaks it.
    exec ${pkgs.systemd}/bin/systemd-inhibit --what=idle --who=cast \
      --why='Casting the screen to a TV' \
      ${config.services.sunshine.package}/bin/sunshine ${configFile} output_name="$output"
  '';
in {
  services.sunshine = {
    enable = true;

    # It captures the screen and injects input, so `cast` starts and stops it
    # rather than leaving it up for the session.
    autoStart = false;

    settings = {
      # KMS capture cannot see a headless output.  If the stream ever comes
      # through black, see Sunshine#5258 in the design notes.
      capture = "wlr";

      # The web UI answers any LAN host by default.  This rejects remote
      # requests; it does not change what Sunshine binds, so the port stays
      # closed below too.
      origin_web_ui_allowed = "pc";
    };
  };

  systemd.user.services.sunshine.serviceConfig.ExecStart = lib.mkForce sunshine-cast;

  # `openFirewall`'s own list minus the web UI at +1, which pairing does not
  # gate.  Written out so re-adding it is visible in review.
  networking.firewall = {
    # -5 nvhttp HTTPS, 0 nvhttp HTTP, 21 RTSP.  (+1 is the web UI.)
    allowedTCPPorts = mkPorts [(-5) 0 21];

    # 9 video, 10 control, 11 audio, 13 mic, 21 RTSP.
    allowedUDPPorts = mkPorts [9 10 11 13 21];
  };

  # Enabling Sunshine also turns on Avahi publishing system-wide, so lyra
  # advertises itself wherever it goes.  That is what Moonlight discovers.
}
