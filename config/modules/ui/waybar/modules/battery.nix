{colors}: {
  name = "battery";

  config = {
    states = {
      warning = 15;
      critical = 5;
    };
    format = "{icon} {capacity}%";
    format-charging = "{icon} 󰚥 {capacity}%";
    format-plugged = "󰚥 {capacity}%";
    format-alt = "{icon} {time}";
    format-icons = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
  };

  style = {
    ".warning".color = colors.warn;
    ".critical" = {
      background-color = colors.urgent;
      color = colors.white;
    };
    ".critical:not(.charging)" = {
      animation-name = "blink";
      animation-duration = "0.5s";
      animation-timing-function = "linear";
      animation-iteration-count = "infinite";
      animation-direction = "alternate";
    };
  };
}
