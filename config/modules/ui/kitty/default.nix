{
  pkgs,
  config,
  ...
}: {
  primary-user.home-manager = {
    default-terminal = {
      enable = true;
      bin = "${pkgs.kitty}/bin/kitty";
      pkg = pkgs.kitty;
      termname = "xterm-kitty";
    };

    programs.kitty = {
      enable = config.primary-user.home-manager.default-terminal.enableApplication;
      settings = {
        open_url_with = "${pkgs.launcher}/bin/browse";
        remember_window_size = "no";
      };
      keybindings = {
        "ctrl+plus" = "change_font_size all +1.0";
        "ctrl+minus" = "change_font_size all -1.0";
        "ctrl+equal" = "change_font_size all 0";
      };
    };
  };
}
