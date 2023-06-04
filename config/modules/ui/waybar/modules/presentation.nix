{
  pkgs,
  colors,
}: let
  presentation = pkgs.writeShellScript "presentation" ''
    if ! systemctl --user is-active --quiet swayidle
    then
      echo 󰐯
    fi
  '';
in {
  name = "custom/presentation";

  config = {
    exec = presentation;
    interval = 1;
  };

  style."" = {
    background-color = colors.selection;
    color = colors.background;
    font-weight = "bold";
  };
}
