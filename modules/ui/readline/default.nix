{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.home.file.".inputrc".text = "set editing-mode vi";
}
