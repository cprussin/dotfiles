{
  pkgs,
  config,
  ...
}: {
  primary-user.home-manager.services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 330;
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        timeout = 300;
        command = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        resumeCommand = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        event = "lock";
        command = "(${config.security.wrapperDir}/sudo ${config.primary-user.secure.exportCmd}; ${pkgs.swaylock}/bin/swaylock)";
      }
    ];
  };
}
