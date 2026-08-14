{
  config,
  lib,
  pkgs,
  ...
}: let
  # How long the session must be idle before the screen blanks (DPMS off).
  screenTimeout = 10 * 60;

  # The screen locks shortly after it blanks, rather than at the same instant.
  lockDelayAfterScreenTimeout = 30;
  lockTimeout = screenTimeout + lockDelayAfterScreenTimeout;

  dpmsOn = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
  resumeCommand =
    if config.screen-idle.dpmsResumeCommand == ""
    then dpmsOn
    else "${dpmsOn}; ${config.screen-idle.dpmsResumeCommand}";
in {
  options.screen-idle.dpmsResumeCommand = lib.mkOption {
    type = lib.types.str;
    default = "";
    description = ''
      Extra shell command to run when the screen wakes from DPMS off, right
      after outputs are re-enabled.  Intended for machines that need to nudge
      an external display driver back to life on resume (e.g. restarting the
      DisplayLink manager so evdi outputs re-initialize).
    '';
  };

  config.primary-user.home-manager.services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = lockTimeout;
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        timeout = screenTimeout;
        command = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        inherit resumeCommand;
      }
    ];
    events = {
      before-sleep = "${pkgs.systemd}/bin/loginctl lock-session";
      lock = "${pkgs.swaylock}/bin/swaylock";
    };
  };
}
