{ ... }:

{
  primary-user.home-manager.programs.swaylock = {
    enable = true;
    daemonize = true;
    image = ../sway/background.png;
    indicator-radius = 200;
    indicator-thickness = 50;
  };
  security.pam.services.swaylock = { };
}
