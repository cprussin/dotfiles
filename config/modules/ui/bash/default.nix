{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.programs.bash.enable = true;
}
