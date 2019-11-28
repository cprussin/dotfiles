{ config }:

{
  name = "sway/workspaces";

  config = {
    disable-scroll = true;
    format = "{name}";
  };

  style = {
    " button" = {
      color = config.colorTheme.bright;
      padding = "0 5px";
      margin-right = "10px";
      border-radius = "0";
      transition = "background-color .25s, color .25s";
    };
    " button:not(.focused):not(.urgent):hover" = {
      background = "rgba(0, 0, 0, 0.5)";
      box-shadow = "inherit";
      text-shadow = "inherit";
      border = "1px solid rgba(0, 0, 0, 0.0)";
    };
    " button.focused" = {
      background = config.colorTheme.selection;
      color = config.colorTheme.background;
    };
    " button.focused:hover" = {
      background = config.colorTheme.selection;
      box-shadow = "inherit";
      text-shadow = "inherit";
      border = "1px solid rgba(0, 0, 0, 0.0)";
    };
    " button.urgent:not(:hover)" = {
      background = config.colorTheme.urgent;
      color = config.colorTheme.white;
      animation-name = "blink";
      animation-duration = "0.5s";
      animation-timing-function = "linear";
      animation-iteration-count = "infinite";
      animation-direction = "alternate";
    };
    " button.urgent:hover" = {
      background = config.colorTheme.urgent;
      color = config.colorTheme.white;
      box-shadow = "inherit";
      text-shadow = "inherit";
      border = "1px solid rgba(0, 0, 0, 0.0)";
    };
  };
}
