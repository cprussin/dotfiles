{ pkgs, config, ... }:
let
  launcher-apps = config.primary-user.home-manager.programs.launcher.apps;
in
{
  primary-user.home-manager.services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 300;
        cmd = launcher-apps.lock;
      }
      {
        timeout = 300;
        cmd = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        resume = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
      }
    ];
    beforeSleep = launcher-apps.lock;
    lock = launcher-apps.lock;
  };
}
