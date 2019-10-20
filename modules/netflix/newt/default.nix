{ config, ... }:

{
  home-manager.users.${config.primaryUserName} = { ... }: {
    home.file.".newt.yml".text = ''
      auto-upgrade:
        enabled: false
    '';
  };
}
