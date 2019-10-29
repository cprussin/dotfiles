{ pkgs, config, ... }:

let
  fontLib = pkgs.callPackage ../../../lib/fonts.nix {};
in

{
  nixpkgs.overlays = [
    (
      _: super: {
        dunst = super.dunst.override {
          dunstify = true;
        };
        mk-progress-string = super.callPackage ./mk-progress-string.nix {};
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.services.dunst = {
    enable = true;

    iconTheme = config.iconTheme;

    settings = {
      global = {
        markup = "full";
        font = fontLib.pangoFont config.fontTheme.primaryFont;
        icon_position = "left";
        max_icon_size = 32;
        sort = "yes";
        indicate_hidden = "yes";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ignore_newline = "no";
        geometry = "300x5+30-50";
        transparency = 0;
        idle_threshold = 120;
        monitor = 0;
        follow = "mouse";
        sticky_history = "yes";
        line_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        separator_color = "frame";
        startup_notification = "false";
        frame_width = 3;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+dollar";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = config.colorTheme.background;
        foreground = config.colorTheme.foreground;
        frame_color = config.colorTheme.highlightBackground;
        timeout = 10;
      };
      urgency_normal = {
        background = config.colorTheme.background;
        foreground = config.colorTheme.bright;
        frame_color = config.colorTheme.highlightBackground;
        timeout = 10;
      };
      urgency_critical = {
        background = config.colorTheme.background;
        foreground = config.colorTheme.urgent;
        frame_color = config.colorTheme.urgent;
        timeout = 0;
      };
    };
  };
}
