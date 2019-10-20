{ config, ... }:

{
  home-manager.users.${config.primaryUserName} = homeManager: {
    home.file.".unison/home.prf".text = ''
      label = Sync to home.prussin.net
      root = ${homeManager.config.home.homeDirectory}
      root = ssh://cprussin@home.prussin.net//home/cprussin
      auto = true
      log = false

      path = Backups
      path = Camera
      path = Documents
      path = Music
      path = Notes
      path = Projects
      path = Software

      ignore = Name node_modules
      ignore = Name bower_components
    '';
  };
}
