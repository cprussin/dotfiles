{ config, ... }:

{
  programs.adb.enable = true;
  users.users.${config.primaryUserName}.extraGroups = [ "adbusers" ];
}
