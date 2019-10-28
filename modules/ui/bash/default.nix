{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.programs.bash.enable = true;
}
