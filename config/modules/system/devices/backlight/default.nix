{pkgs, ...}: let
  inherit (pkgs) coreutils;

  # These are wluma's own default thresholds: raw ambient light readings (in
  # lux) are bucketed into these named profiles, and wluma learns a separate
  # brightness preference for each one.  We have to ship a config file anyway
  # to point wluma at the right backlight, so they're spelled out here rather
  # than left implicit.
  alsConfig = pkgs.writeText "wluma-als.toml" ''
    [als.iio]
    path = "/sys/bus/iio/devices"
    thresholds = { 0 = "night", 20 = "dark", 80 = "dim", 250 = "normal", 500 = "bright", 800 = "outdoors" }
  '';

  # wluma wants the sysfs directory of the panel's backlight, which is named
  # after whichever driver registered it (`amdgpu_bl1`, `intel_backlight`,
  # ...) and so isn't a path a profile shared by every laptop can hardcode.
  # Pick out the backlight hanging off the internal panel's DRM connector
  # instead, and name the output after that connector, so the learned data
  # survives the backlight device being renumbered.
  writeConfig = pkgs.writeShellScript "wluma-write-config" ''
    panel=
    output=

    for backlight in /sys/class/backlight/*
    do
      [ -e "$backlight/device" ] || continue
      connector=$(${coreutils}/bin/basename "$(${coreutils}/bin/readlink -f "$backlight/device")")
      case "$connector" in
        *-eDP-*)
          panel=$backlight
          output=''${connector#*-}
          break
          ;;
      esac
    done

    # Not every driver registers its backlight against a DRM connector
    # (`acpi_video0` hangs off the ACPI device, and older drivers off the pci
    # device), so where there's only one backlight to begin with, take it.
    if [ -z "$panel" ]
    then
      set -- /sys/class/backlight/*
      if [ $# -eq 1 ] && [ -d "$1" ]
      then
        panel=$1
        output=$(${coreutils}/bin/basename "$1")
      fi
    fi

    if [ -z "$panel" ]
    then
      echo "Found no internal panel backlight under /sys/class/backlight!" >&2
      exit 1
    fi

    {
      ${coreutils}/bin/cat ${alsConfig}

      # Ambient light alone decides the brightness.  wluma can also factor in
      # how bright the screen contents are, but that isn't what macos-style
      # auto-brightness does, and it means capturing the screen several times
      # a second.
      ${coreutils}/bin/printf \
        '\n[[output.backlight]]\nname = "%s"\npath = "%s"\ncapturer = "none"\n' \
        "$output" "$panel"
    } > "$RUNTIME_DIRECTORY/config.toml"
  '';
in {
  primary-user.extraGroups = ["video"];

  # brightnessctl ships udev rules granting the `video` group write access to
  # /sys/class/backlight/*/brightness.  wluma works without them by going
  # through logind over dbus, but only direct writes are quick enough for it
  # to ramp the backlight smoothly.  The same rules also hand the `input`
  # group write access to /sys/class/leds/*/brightness, which nothing here
  # needs but which comes along with them.
  services.udev.packages = [pkgs.brightnessctl];

  # wluma doesn't do anything until it has been taught: adjust the brightness
  # by hand a few times in different lighting and it starts reproducing those
  # choices on its own.  That training lives in ~/.local/share/wluma, which
  # has to be persisted on machines with an ephemeral root.
  primary-user.home-manager.systemd.user.services.wluma = {
    Unit = {
      Description = "Adjust the backlight to the ambient light level";
      PartOf = ["graphical-session.target"];
      After = ["graphical-session.target"];

      # wluma panics rather than degrades when the sensor isn't there, and
      # the script above bails when the panel isn't either, but the sensor
      # may genuinely not be bound yet at login, so retries are worth having.
      # The default limit of five starts per ten seconds can't catch a five
      # second restart delay, so widen the window enough that a sensor that
      # never turns up leaves the unit failed instead of backtracing into the
      # journal every five seconds forever.  Only failures to start are
      # bounded this way: wluma also panics when it can't save what it has
      # learned, and those are as far apart as the brightness keypresses that
      # provoke them.  A spent burst refuses every start for the rest of the
      # window, so recovering inside one takes a `reset-failed`.
      StartLimitIntervalSec = 300;
      StartLimitBurst = 5;
    };

    Service = {
      Type = "simple";

      # wluma only reads its config from $XDG_CONFIG_HOME/wluma/config.toml,
      # and we generate that config at startup, so point it at the unit's
      # runtime directory for the duration.
      RuntimeDirectory = "wluma";
      Environment = ["XDG_CONFIG_HOME=%t"];

      ExecStartPre = "${writeConfig}";
      ExecStart = "${pkgs.wluma}/bin/wluma";
      Restart = "on-failure";
      RestartSec = "5s";
    };

    Install.WantedBy = ["graphical-session.target"];
  };
}
