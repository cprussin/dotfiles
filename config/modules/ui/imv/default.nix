{ config, ... }:

let
  imvColor = builtins.replaceStrings [ "#" ] [ "" ];
  font = config.primary-user.home-manager.font;
  colors = config.primary-user.home-manager.colorTheme;
in

{
  primary-user.home-manager.xdg.configFile."imv/config".text = ''
    [options]
    background = ${imvColor colors.background}
    overlay_font = ${font.face}:${toString font.size}

    [binds]
    n = next
    p = prev
    <Ctrl+plus> = zoom 1
    <plus> = zoom 1
    <Ctrl+minus> = zoom -1
    <minus> = zoom -1
    <Ctrl+equal> = zoom actual
    <equal> = zoom actual
  '';
}
