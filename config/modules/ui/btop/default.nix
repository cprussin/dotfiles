{ pkgs, ... }: {
  primary-user.home-manager.programs.btop = {
    enable = true;
    settings = {
      color_theme = "${pkgs.btop}/share/btop/themes/solarized_dark.theme";
      theme_background = false;
      vim_keys = true;
      proc_tree = true;
      io_mode = true;
      proc_sorting = "pid";
    };
  };
}
