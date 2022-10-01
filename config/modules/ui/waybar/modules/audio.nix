{
  colors,
  pavucontrol,
  pamixer,
}: {
  name = "pulseaudio#sink";

  config = {
    format = "{icon} {volume}%";
    format-bluetooth = "{icon} {volume}%";
    format-bluetooth-muted = "{icon} {volume}%";
    format-muted = "婢 {volume}%";
    format-icons = {
      headphones = "";
      handsfree = "";
      headset = "";
      phone = "";
      portable = "";
      car = "";
      default = ["奄" "奔" "墳"];
    };
    on-click = "${pavucontrol}/bin/pavucontrol -t 3";
    on-click-middle = "${pamixer}/bin/pamixer --toggle-mute";
  };

  style.".muted".color = colors.foreground;
}
