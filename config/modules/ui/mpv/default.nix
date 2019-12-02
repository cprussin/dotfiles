{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.programs.mpv = {
    enable = true;
    config = {
      gpu-context = "wayland";
    };
  };
}
