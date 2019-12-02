{ config, ... }:

{
  home-manager.users.${config.primary-user.name}.home.file.".newt.yml".text = ''
    auto-upgrade:
      enabled: false
  '';
}
