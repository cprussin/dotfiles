{
  config,
  colors,
}: {
  name = "network#wireless";

  config = {
    interface = config.interfaces.wifi;
    interval = "5";
    format = "";
    format-disconnected = "";
    format-linked = "";
    format-wifi = " ({essid}) {ipaddr} [{signalStrength}%]";
    format-alt = " {ifname}: ({essid}) {ipaddr}/{cidr} {frequency}mhz [{signalStrength}%]";
  };

  style.":not(.wifi)".color = colors.foreground;
}
