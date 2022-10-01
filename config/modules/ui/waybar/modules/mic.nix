{
  pavucontrol,
  colors,
  pamixer,
}: {
  name = "pulseaudio#source";

  config = {
    format = "{format_source}";
    format-source = " {volume}%";
    format-source-muted = "<span color=\"${colors.foreground}\"> {volume}%</span>";
    on-scroll-up = "${pamixer}/bin/pamixer --default-source --increase 1";
    on-scroll-down = "${pamixer}/bin/pamixer --default-source --decrease 1";
    on-click = "${pavucontrol}/bin/pavucontrol -t 4";
    on-click-middle = "${pamixer}/bin/pamixer --default-source --toggle-mute";
  };
}
