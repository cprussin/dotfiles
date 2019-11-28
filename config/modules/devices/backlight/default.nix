{ config, ... }:

{
  hardware.brightnessctl.enable = true;
  users.users.${config.primaryUserName}.extraGroups = [ "video" ];
}
