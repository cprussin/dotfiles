{ ... }:

{
  programs.adb.enable = true;
  primary-user.extraGroups = [ "adbusers" ];
}
