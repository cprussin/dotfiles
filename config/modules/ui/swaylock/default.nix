_: {
  primary-user.home-manager.programs.swaylock.settings = {
    daemonize = true;
    image = toString ../sway/background.png;
    indicator-radius = 200;
    indicator-thickness = 50;
  };
  security.pam.services.swaylock = {};
}
