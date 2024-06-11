_: {
  primary-user.home-manager.programs.swaylock = {
    enable = true;
    settings = {
      daemonize = true;
      image = builtins.toPath ../sway/background.png;
      indicator-radius = 200;
      indicator-thickness = 50;
    };
  };
  security.pam.services.swaylock = {};
}
