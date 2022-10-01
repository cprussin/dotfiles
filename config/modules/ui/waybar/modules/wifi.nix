{
  config,
  colors,
}: {
  name = "network#wireless";

  config = {
    interface = config.interfaces.wifi;
    interval = "5";
    format = "直";
    format-disconnected = "睊";
    format-linked = "直";
    format-wifi = "直 ({essid}) {ipaddr} [{signalStrength}%]";
    format-alt = "直 {ifname}: ({essid}) {ipaddr}/{cidr} {frequency}mhz [{signalStrength}%]";
  };

  style.":not(.wifi)".color = colors.foreground;
}
