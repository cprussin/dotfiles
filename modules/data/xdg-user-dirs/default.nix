{ config, ... }:

{
  xdg.configFile."user-dirs.dirs".text = ''
    XDG_DESKTOP_DIR="${config.home.homeDirectory}/"
    XDG_DOWNLOAD_DIR="${config.home.homeDirectory}/Scratch"
    XDG_TEMPLATES_DIR="${config.home.homeDirectory}/"
    XDG_PUBLICSHARE_DIR="${config.home.homeDirectory}/"
    XDG_DOCUMENTS_DIR="${config.home.homeDirectory}/Documents"
    XDG_MUSIC_DIR="${config.home.homeDirectory}/Music"
    XDG_PICTURES_DIR="${config.home.homeDirectory}/Camera"
    XDG_VIDEOS_DIR="${config.home.homeDirectory}/Camera"
  '';
}
