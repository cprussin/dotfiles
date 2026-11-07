# Switch power-profiles-daemon to `power-saver` on battery and back to
# `balanced` on AC.  Runs at boot and on every charger plug/unplug; a profile
# picked by hand holds until the next one.
#
# Only `Mains` supplies count: on the Framework that is `ACAD`, which tracks the
# charger whichever port it is in.  The USB-C `ucsi-source-psy-*` supplies are
# type `USB` and are ignored.
{
  config,
  pkgs,
  ...
}: let
  set-profile = pkgs.writeShellScript "power-profile-on-battery" ''
    profile=power-saver
    for supply in /sys/class/power_supply/*
    do
      [ "$(${pkgs.coreutils}/bin/cat "$supply/type" 2>/dev/null)" = Mains ] || continue
      [ "$(${pkgs.coreutils}/bin/cat "$supply/online" 2>/dev/null)" = 1 ] && profile=balanced
    done
    exec ${config.services.power-profiles-daemon.package}/bin/powerprofilesctl set "$profile"
  '';
in {
  systemd.services.power-profile-on-battery = {
    description = "Pick the power profile for the current power source";
    wantedBy = ["multi-user.target"];
    requires = ["power-profiles-daemon.service"];
    after = ["power-profiles-daemon.service"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = set-profile;
    };
  };

  # `--no-block` so udev never waits on D-Bus.
  services.udev.extraRules = ''
    SUBSYSTEM=="power_supply", ATTR{type}=="Mains", RUN+="${pkgs.systemd}/bin/systemctl --no-block start power-profile-on-battery.service"
  '';
}
