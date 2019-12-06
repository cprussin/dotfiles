{ config, ... }:

{
  primary-user.home-manager.programs.mako = {
    enable = true;
    background-color = "${config.colorTheme.foreground}E0";
    text-color = config.colorTheme.background;
    border-size = 3;
    border-color = config.colorTheme.highlightForeground;
    padding = 20;
    margin = 30;
    width = 500;
    height = 600;
    progress-color = "source ${config.colorTheme.bright}";
    default-timeout = 10000;
  };
}
