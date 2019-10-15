{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      dunst = super.dunst.override {
       dunstify = true;
      };
      mk-progress-string = super.callPackage ./mk-progress-string.nix {};
    })
  ];

  services.dunst = {
    enable = true;

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
      size = "48x48";
    };

    settings = {
      global = {
        allow_markup = "yes";
        font = "${config.primaryFont.face} ${toString config.primaryFont.size}";
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
        background = "#002b36";
        foreground = "#888888";
        frame_color = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#002b36";
        foreground = "#268bd2";
        frame_color = "#073642";
        timeout = 10;
      };
      urgency_critical = {
        background = "#002b36";
        foreground = "#d33682";
        frame_color = "#d33682";
        timeout = 0;
      };
    };
  };
}
