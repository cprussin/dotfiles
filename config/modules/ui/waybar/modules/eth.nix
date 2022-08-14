{
  config,
  colors,
}: let
  icon = import ../icon.nix;
in {
  name = "network#eth";

  config = {
    interface = config.interfaces.eth;
    interval = "5";
    format = icon "";
    format-disconnected = icon "";
    format-linked = icon "";
    format-ethernet = "${icon ""} {ipaddr}";
    format-alt = "${icon ""} {ifname}: {ipaddr}/{cidr}";
  };

  style.":not(.ethernet)".color = colors.foreground;
}
