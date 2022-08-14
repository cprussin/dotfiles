{
  pkgs,
  config,
  ...
}: let
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;
in {
  primary-user.home-manager.services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 300;
        command = toString launcher-apps.lock;
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
        command = toString launcher-apps.lock;
      }
      {
        event = "lock";
        command = toString launcher-apps.lock;
      }
    ];
  };
}
