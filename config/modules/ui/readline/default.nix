{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.home.file.".inputrc".text = "set editing-mode vi";
}
