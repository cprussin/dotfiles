{ pkgs, config, lib, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
in

{
  primary-user.home-manager = {
    default-terminal = "${pkgs.termite}/bin/termite";

    home.packages = lib.mkForce [
      pkgs.termite.terminfo
    ];

    programs.termite = {
      enable = true;
      dynamicTitle = true;
      scrollbackLines = -1;
      font = fontLib.pangoFont config.fontTheme.primaryFont;
      backgroundColor = config.colorTheme.background;
      foregroundColor = config.colorTheme.foreground;
      hintsActiveBackgroundColor = config.colorTheme.selection;
      hintsActiveForegroundColor = config.colorTheme.background;
      hintsBackgroundColor = config.colorTheme.highlightBackground;
      hintsForegroundColor = config.colorTheme.highlightForeground;
      hintsBorderColor = config.colorTheme.highlightForeground;
      hintsBorderWidth = "1";
      hintsFont = fontLib.pangoFont config.fontTheme.primaryFont;
      hintsPadding = 3;
      hintsRoundness = "0";
      cursorColor = config.colorTheme.foreground;
      cursorForegroundColor = config.colorTheme.background;
      colorsExtra = ''
        color0 = ${config.colorTheme.black}
        color1 = ${config.colorTheme.red}
        color2 = ${config.colorTheme.green}
        color3 = ${config.colorTheme.yellow}
        color4 = ${config.colorTheme.blue}
        color5 = ${config.colorTheme.purple}
        color6 = ${config.colorTheme.cyan}
        color7 = ${config.colorTheme.lightGrey}
        color8 = ${config.colorTheme.grey}
        color9 = ${config.colorTheme.lightRed}
        color10 = ${config.colorTheme.lightGreen}
        color11 = ${config.colorTheme.lightYellow}
        color12 = ${config.colorTheme.lightBlue}
        color13 = ${config.colorTheme.lightPurple}
        color14 = ${config.colorTheme.lightCyan}
        color15 = ${config.colorTheme.white}
      '';
    };

    programs.zsh.initExtra = ''
      if [[ $TERM = xterm-termite ]]; then
        precmd_functions+=(__vte_prompt_command)
      fi
    '';
  };
}
