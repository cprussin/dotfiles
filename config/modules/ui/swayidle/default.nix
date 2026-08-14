{pkgs, ...}: let
  # How long the session must be idle before the screen blanks (DPMS off).
  screenTimeout = 10 * 60;

  # The screen locks shortly after it blanks, rather than at the same instant.
  lockDelayAfterScreenTimeout = 30;
  lockTimeout = screenTimeout + lockDelayAfterScreenTimeout;
in {
  primary-user.home-manager.services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = lockTimeout;
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        timeout = screenTimeout;
        command = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        resumeCommand = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
      }
    ];
    events = {
      before-sleep = "${pkgs.systemd}/bin/loginctl lock-session";
      lock = "${pkgs.swaylock}/bin/swaylock";
    };
  };
}
