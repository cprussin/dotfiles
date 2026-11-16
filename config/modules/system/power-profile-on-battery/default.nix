# Switch power-profiles-daemon to `power-saver` on battery and back to
# `balanced` on AC.  Runs at boot and on every charger plug/unplug; a profile
# picked by hand holds until the next one.
#
# power-profiles-daemon ships its `amdgpu_panel_power` action switched off, so
# the panel's adaptive backlight (ABM), which dims the backlight and boosts
# contrast to match, never came on.  With it on, the daemon turns ABM up on
# battery as the charge falls: in power-saver from below 50%, in balanced from
# below 30%.  The daemon keeps that choice under /var/lib, which a tmpfs root
# forgets, so switch it on whenever it's off (switching restarts the daemon's
# drivers, so leave it be when it's already on).
#
# Only `Mains` supplies count: on the Framework that is `ACAD`, which tracks the
# charger whichever port it is in.  The USB-C `ucsi-source-psy-*` supplies are
# type `USB` and are ignored.
{
  config,
  pkgs,
  ...
}: let
  powerprofilesctl = "${config.services.power-profiles-daemon.package}/bin/powerprofilesctl";
  set-profile = pkgs.writeShellScript "power-profile-on-battery" ''
    profile=power-saver
    for supply in /sys/class/power_supply/*
    do
      [ "$(${pkgs.coreutils}/bin/cat "$supply/type" 2>/dev/null)" = Mains ] || continue
      [ "$(${pkgs.coreutils}/bin/cat "$supply/online" 2>/dev/null)" = 1 ] && profile=balanced
    done
    ${powerprofilesctl} list-actions \
      | ${pkgs.gawk}/bin/awk -v RS= '{ $0 = tolower($0) } /name: amdgpu_panel_power/ && /enabled: true/ { found = 1 } END { exit !found }' \
      || ${powerprofilesctl} configure-action --enable amdgpu_panel_power
    exec ${powerprofilesctl} set "$profile"
  '';
in {
  systemd.services.power-profile-on-battery = {
    description = "Pick the power profile for the current power source";
    # The daemon starts after multi-user.target and is wanted by
    # graphical.target, so being wanted by multi-user.target while ordered
    # after the daemon is a cycle, and systemd drops this unit at boot.
    wantedBy = ["graphical.target"];
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
