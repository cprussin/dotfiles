{ config }:

{
  name = "network#eth";

  config = {
    interface = "enp0s31f6";
    interval = "5";
    format = "";
    format-disconnected = "";
    format-linked = "";
    format-ethernet = " {ipaddr}";
    format-alt = " {ifname}: {ipaddr}/{cidr}";
  };

  style.":not(.ethernet)".color = config.colorTheme.foreground;
}
