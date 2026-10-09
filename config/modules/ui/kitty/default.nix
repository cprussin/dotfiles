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
        # The desktop's `BROWSER`: `domicile-open-url` in domicile, `browse`
        # elsewhere.  Not `xdg-open`, which outside domicile is launcher's
        # `run`, and that `eval`s its argument.
        open_url_with = "${pkgs.writeShellScript "kitty-open-url" ''
          exec "''${BROWSER:-${pkgs.launcher}/bin/browse}" "$@"
        ''}";
        remember_window_size = "no";
        confirm_os_window_close = "0";
      };
      keybindings = {
        "ctrl+plus" = "change_font_size all +1.0";
        "ctrl+minus" = "change_font_size all -1.0";
        "ctrl+equal" = "change_font_size all 0";
      };
    };
  };
}
