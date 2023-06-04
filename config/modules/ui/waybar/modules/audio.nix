{
  colors,
  pavucontrol,
  pamixer,
}: {
  name = "pulseaudio#sink";

  config = {
    format = "{icon} {volume}%";
    format-bluetooth = "{icon}󰂰 {volume}%";
    format-bluetooth-muted = "󰖁󰂰 {volume}%";
    format-muted = "󰖁 {volume}%";
    format-icons = {
      headphones = "󰋋";
      handsfree = "󰋎";
      headset = "󰋎";
      phone = "";
      portable = "";
      car = "󰄋";
      default = ["󰕿" "󰖀" "󰕾"];
    };
    on-click = "${pavucontrol}/bin/pavucontrol -t 3";
    on-click-middle = "${pamixer}/bin/pamixer --toggle-mute";
  };

  style.".muted".color = colors.foreground;
}
