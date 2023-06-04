{
  config,
  colors,
}: {
  name = "network#eth";

  config = {
    interface = config.interfaces.eth;
    interval = "5";
    format = "󰈁";
    format-disconnected = "󰈂";
    format-linked = "󰈁";
    format-ethernet = "󰈁 {ipaddr}";
    format-alt = "󰈁 {ifname}: {ipaddr}/{cidr}";
  };

  style.":not(.ethernet)".color = colors.foreground;
}
