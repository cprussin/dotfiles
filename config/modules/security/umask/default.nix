{ config, ... }:
let
  umask = 077;
  umaskCmd = "umask ${toString umask}";
in
{
  primary-user.home-manager = {
    home.file = {
      ".profile".text = umaskCmd;
      ".bashrc".text = umaskCmd;
    };
    programs.zsh.initExtra = umaskCmd;
  };
  systemd.services."home-manager-${config.primary-user.name}".serviceConfig.UMask = umask;
}
