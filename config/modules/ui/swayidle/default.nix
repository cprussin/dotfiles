{pkgs, ...}: {
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
    events = {
      before-sleep = "${pkgs.systemd}/bin/loginctl lock-session";
      lock = "${pkgs.swaylock}/bin/swaylock";
    };
  };
}
