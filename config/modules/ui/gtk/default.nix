{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.gtk.enable = true;
}
