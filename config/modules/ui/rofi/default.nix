{ config, ... }:

let
  colors = config.colorTheme;
in

{
  primary-user.home-manager.programs.rofi = {
    enable = true;
    borderWidth = 3;
    extraConfig = ''
      rofi.fixed-num-lines: true
      rofi.color-normal: ${colors.background},${colors.foreground},${colors.background},${colors.selection},${colors.background}
      rofi.color-window: ${colors.background},${colors.selection}
      rofi.disable-history: true
      rofi.modi: run
    '';
  };
}
