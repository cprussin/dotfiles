{ config }:

{
  name = "sway/mode";

  config.format = "{}";

  style."" = {
    background-color = config.colorTheme.highlightBackground;
    color = config.colorTheme.highlightForeground;
    font-weight = "bold";
  };
}
