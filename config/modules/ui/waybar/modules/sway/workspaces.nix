{ colors }:

{
  name = "sway/workspaces";

  config = {
    disable-scroll = true;
    format = "{name}";
  };

  style = {
    " button" = {
      color = colors.bright;
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
      background = colors.selection;
      color = colors.background;
    };
    " button.focused:hover" = {
      background = colors.selection;
      box-shadow = "inherit";
      text-shadow = "inherit";
      border = "1px solid rgba(0, 0, 0, 0.0)";
    };
    " button.urgent:not(:hover)" = {
      background = colors.urgent;
      color = colors.white;
      animation-name = "blink";
      animation-duration = "0.5s";
      animation-timing-function = "linear";
      animation-iteration-count = "infinite";
      animation-direction = "alternate";
    };
    " button.urgent:hover" = {
      background = colors.urgent;
      color = colors.white;
      box-shadow = "inherit";
      text-shadow = "inherit";
      border = "1px solid rgba(0, 0, 0, 0.0)";
    };
  };
}
