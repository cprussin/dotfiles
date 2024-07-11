{
  config,
  colors,
  iwgtk,
  lib,
}: {
  name = "network#wireless";

  config = {
    interface = config.interfaces.wifi;
    interval = "5";
    format = "{icon}";
    format-disconnected = "󰤭";
    format-linked = "{icon} [No IP]";
    format-wifi = "{icon} ({essid}) [{signalStrength}%]";
    tooltip-format = "{icon} {ifname}: ({essid}) {ipaddr}/{cidr} {frequency}mhz [{signalStrength}%]";
    format-icons = ["󰤟" "󰤢" "󰤥" "󰤨"];
    on-click = lib.getExe iwgtk;
  };

  style.":not(.wifi)".color = colors.foreground;
}
