{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.programs.mpv = {
    enable = true;
    config = {
      gpu-context = "wayland";
    };
  };
}
