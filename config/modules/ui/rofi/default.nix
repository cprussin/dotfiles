{ pkgs, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
  colors = config.colorTheme;
in

{
  home-manager.users.${config.primary-user.name}.xdg.configFile."rofi/config".text = ''
    rofi.fixed-num-lines: true
    rofi.font: ${fontLib.pangoFont config.fontTheme.primaryFont}
    rofi.color-normal: ${colors.background},${colors.foreground},${colors.background},${colors.selection},${colors.background}
    rofi.color-window: ${colors.background},${colors.selection}
    rofi.bw: 3
    rofi.location: 0
    rofi.disable-history: true
    rofi.modi: run
  '';
}
