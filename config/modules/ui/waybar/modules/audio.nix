{
  colors,
  pavucontrol,
  pamixer,
}: let
  icon = import ../icon.nix;
in {
  name = "pulseaudio#sink";

  config = {
    format = "{icon} {volume}%";
    format-bluetooth = "{icon}${icon ""} {volume}%";
    format-bluetooth-muted = "{icon}${icon ""} ${icon ""} {volume}%";
    format-muted = "${icon ""} {volume}%";
    format-icons = {
      headphones = icon "";
      handsfree = icon "";
      headset = icon "";
      phone = icon "";
      portable = icon "";
      car = icon "";
      default = map icon ["" "" ""];
    };
    on-click = "${pavucontrol}/bin/pavucontrol -t 3";
    on-click-middle = "${pamixer}/bin/pamixer --toggle-mute";
  };

  style.".muted".color = colors.foreground;
}
