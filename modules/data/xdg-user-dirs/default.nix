{ config, ... }:

{
  home-manager.users.${config.primaryUserName} = homeManager: {
    xdg.configFile."user-dirs.dirs".text = ''
      XDG_DESKTOP_DIR="${homeManager.config.home.homeDirectory}/"
      XDG_DOWNLOAD_DIR="${homeManager.config.home.homeDirectory}/Scratch"
      XDG_TEMPLATES_DIR="${homeManager.config.home.homeDirectory}/"
      XDG_PUBLICSHARE_DIR="${homeManager.config.home.homeDirectory}/"
      XDG_DOCUMENTS_DIR="${homeManager.config.home.homeDirectory}/Documents"
      XDG_MUSIC_DIR="${homeManager.config.home.homeDirectory}/Music"
      XDG_PICTURES_DIR="${homeManager.config.home.homeDirectory}/Camera"
      XDG_VIDEOS_DIR="${homeManager.config.home.homeDirectory}/Camera"
    '';
  };
}
