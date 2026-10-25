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
        # `xdg-open`, which domicile answers with `domicile open-url`.
        open_url_with = "default";
        remember_window_size = "no";
        confirm_os_window_close = "0";
        # Reflow on every size during a drag. kitty's default waits 0.1s after
        # the last resize, so its text lags the window frame while resizing.
        resize_debounce_time = "0 0";
      };
      keybindings = {
        "ctrl+plus" = "change_font_size all +1.0";
        "ctrl+minus" = "change_font_size all -1.0";
        "ctrl+equal" = "change_font_size all 0";
      };
    };
  };
}
