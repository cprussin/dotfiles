{
  config,
  colors,
}: let
  icon = import ../icon.nix;
in {
  name = "network#wireless";

  config = {
    interface = config.interfaces.wifi;
    interval = "5";
    format = icon "";
    format-disconnected = icon "";
    format-linked = icon "";
    format-wifi = "${icon ""} ({essid}) {ipaddr} [{signalStrength}%]";
    format-alt = "${icon ""} {ifname}: ({essid}) {ipaddr}/{cidr} {frequency}mhz [{signalStrength}%]";
  };

  style.":not(.wifi)".color = colors.foreground;
}
