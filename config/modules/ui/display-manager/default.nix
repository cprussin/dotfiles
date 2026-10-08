{
  config,
  pkgs,
  ...
}: let
  # SDDM writes a Wayland session's stdout and stderr to
  # `~/.local/share/sddm/wayland-session.log`.  `~` is on the tmpfs root and the
  # file is truncated on every login, so whatever the desktop printed before a
  # crash is gone by the time anyone looks.  Send it to the journal instead,
  # which `/var/log` keeps across boots: `journalctl -b -1 -t wayland-session`.
  #
  # SDDM runs `SessionCommand` with the session's `Exec=` as its arguments, so
  # this wraps the stock script rather than replacing it.
  wayland-session = pkgs.writeShellScript "wayland-session" ''
    exec ${pkgs.systemd}/bin/systemd-cat --identifier=wayland-session \
      ${config.services.displayManager.sddm.package}/share/sddm/scripts/wayland-session "$@"
  '';
in {
  services.displayManager = {
    enable = true;
    sddm = {
      enable = true;
      wayland.enable = true;
      settings.Wayland.SessionCommand = "${wayland-session}";
    };
    autoLogin = {
      enable = true;
      user = config.primary-user.name;
    };
  };
}
