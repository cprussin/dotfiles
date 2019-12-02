{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.gtk.enable = true;
}
