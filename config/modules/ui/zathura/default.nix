{ pkgs, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
in

{
  home-manager.users.${config.primaryUserName}.programs.zathura = {
    enable = true;
    options = {
      default-bg = config.colorTheme.highlightBackground;
      default-fg = config.colorTheme.highlightForeground;
      highlight-color = config.colorTheme.highlightBackground;
      highlight-active-color = config.colorTheme.selection;
      completion-bg = config.colorTheme.highlightBackground;
      completion-fg = config.colorTheme.secondaryContent;
      completion-group-bg = config.colorTheme.highlightForeground;
      completion-group-fg = config.colorTheme.highlightBackground;
      completion-highlight-bg = config.colorTheme.selection;
      completion-highlight-fg = config.colorTheme.highlightBackground;
      inputbar-bg = config.colorTheme.background;
      inputbar-fg = config.colorTheme.foreground;
      statusbar-bg = config.colorTheme.highlightBackground;
      statusbar-fg = config.colorTheme.highlightForeground;
      notification-bg = config.colorTheme.background;
      notification-fg = config.colorTheme.foreground;
      notification-error-bg = config.colorTheme.warn;
      notification-error-fg = config.colorTheme.background;
      notification-warning-bg = config.colorTheme.urgent;
      notification-warning-fg = config.colorTheme.background;
      recolor = true;
      recolor-darkcolor = config.colorTheme.foreground;
      recolor-lightcolor = config.colorTheme.background;
      render-loading-bg = config.colorTheme.highlightBackground;
      render-loading-fg = config.colorTheme.highlightForeground;
      window-title-home-tilde = true;
      window-title-page = true;
      font = fontLib.pangoFont config.fontTheme.primaryFont;
    };
  };
}
