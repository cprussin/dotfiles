{ config, lib, pkgs, ... }:

let
  cfg = config.colorTheme;
  colorThemeType = pkgs.callPackage ../../lib/type/colorTheme.nix {};
  removeOctothorpe = builtins.replaceStrings [ "#" ] [ "" ];
in

{
  options.colorTheme = lib.mkOption {
    type = lib.types.nullOr colorThemeType;
    default = null;
  };

  config = lib.mkIf (cfg != null) {
    programs = {
      termite = {
        backgroundColor = cfg.background;
        foregroundColor = cfg.foreground;
        hintsActiveBackgroundColor = cfg.selection;
        hintsActiveForegroundColor = cfg.background;
        hintsBackgroundColor = cfg.highlightBackground;
        hintsForegroundColor = cfg.highlightForeground;
        hintsBorderColor = cfg.highlightForeground;
        cursorColor = cfg.foreground;
        cursorForegroundColor = cfg.background;
        colorsExtra = ''
          color0 = ${cfg.black}
          color1 = ${cfg.red}
          color2 = ${cfg.green}
          color3 = ${cfg.yellow}
          color4 = ${cfg.blue}
          color5 = ${cfg.purple}
          color6 = ${cfg.cyan}
          color7 = ${cfg.lightGrey}
          color8 = ${cfg.grey}
          color9 = ${cfg.lightRed}
          color10 = ${cfg.lightGreen}
          color11 = ${cfg.lightYellow}
          color12 = ${cfg.lightBlue}
          color13 = ${cfg.lightPurple}
          color14 = ${cfg.lightCyan}
          color15 = ${cfg.white}
        '';
      };

      zathura.options = {
        default-bg = cfg.highlightBackground;
        default-fg = cfg.highlightForeground;
        highlight-color = cfg.highlightBackground;
        highlight-active-color = cfg.selection;
        completion-bg = cfg.highlightBackground;
        completion-fg = cfg.secondaryContent;
        completion-group-bg = cfg.highlightForeground;
        completion-group-fg = cfg.highlightBackground;
        completion-highlight-bg = cfg.selection;
        completion-highlight-fg = cfg.highlightBackground;
        inputbar-bg = cfg.background;
        inputbar-fg = cfg.foreground;
        statusbar-bg = cfg.highlightBackground;
        statusbar-fg = cfg.highlightForeground;
        notification-bg = cfg.background;
        notification-fg = cfg.foreground;
        notification-error-bg = cfg.warn;
        notification-error-fg = cfg.background;
        notification-warning-bg = cfg.urgent;
        notification-warning-fg = cfg.background;
        recolor-darkcolor = cfg.foreground;
        recolor-lightcolor = cfg.background;
        render-loading-bg = cfg.highlightBackground;
        render-loading-fg = cfg.highlightForeground;
      };

      swaylock = {
        color = removeOctothorpe cfg.background;
        text-caps-lock-color = removeOctothorpe cfg.background;
        text-ver-color = removeOctothorpe cfg.background;
        text-wrong-color = removeOctothorpe cfg.background;
        bs-hl-color = removeOctothorpe cfg.warn;
        caps-lock-bs-hl-color = removeOctothorpe cfg.urgent;
        caps-lock-key-hl-color = removeOctothorpe cfg.selection;
        inside-color = "00000000";
        inside-clear-color = removeOctothorpe cfg.warn + "D0";
        inside-caps-lock-color = removeOctothorpe cfg.urgent + "D0";
        inside-ver-color = removeOctothorpe cfg.selection + "D0";
        inside-wrong-color = removeOctothorpe cfg.urgent + "D0";
        ring-color = removeOctothorpe cfg.bright;
        ring-clear-color = removeOctothorpe cfg.warn;
        ring-caps-lock-color = removeOctothorpe cfg.urgent;
        ring-ver-color = removeOctothorpe cfg.selection;
        ring-wrong-color = removeOctothorpe cfg.urgent;
        line-color = removeOctothorpe cfg.highlightBackground;
        line-clear-color = removeOctothorpe cfg.highlightBackground;
        line-caps-lock-color = removeOctothorpe cfg.highlightBackground;
        line-ver-color = removeOctothorpe cfg.highlightBackground;
        line-wrong-color = removeOctothorpe cfg.highlightBackground;
        separator-color = removeOctothorpe cfg.highlightBackground;
      };

      mako = {
        background-color = "${cfg.foreground}E0";
        text-color = cfg.background;
        border-color = cfg.highlightForeground;
        progress-color = "source ${cfg.bright}";
      };

      waybar.styles.frame = {
        color = cfg.bright;
        background = cfg.background;
      };

      zsh.initExtra = "eval $(dircolors ${config.colorTheme.dircolors})";

      fzf.colors = {
        fg = cfg.foreground;
        bg = cfg.background;
        hl = cfg.bright;
        "fg+" = cfg.highlightForeground;
        "bg+" = cfg.highlightBackground;
        "hl+" = cfg.bright;
        info = cfg.bright;
        prompt = cfg.selection;
        pointer = cfg.selection;
        marker = cfg.selection;
        spinner = cfg.selection;
        header = cfg.secondaryContent;
        gutter = cfg.highlightBackground;
      };
    };
  };
}
