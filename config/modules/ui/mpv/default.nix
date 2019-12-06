{ ... }:

{
  primary-user.home-manager.programs.mpv = {
    enable = true;
    config = {
      gpu-context = "wayland";
    };
  };
}
