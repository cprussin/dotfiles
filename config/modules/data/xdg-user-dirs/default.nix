{ config, ... }:

let
  home = config.primary-user.home;
in

{
  primary-user.home-manager.xdg.configFile."user-dirs.dirs".text = ''
    XDG_DESKTOP_DIR="${home}/"
    XDG_DOWNLOAD_DIR="${home}/Scratch"
    XDG_TEMPLATES_DIR="${home}/"
    XDG_PUBLICSHARE_DIR="${home}/"
    XDG_DOCUMENTS_DIR="${home}/Documents"
    XDG_MUSIC_DIR="${home}/Music"
    XDG_PICTURES_DIR="${home}/Camera"
    XDG_VIDEOS_DIR="${home}/Camera"
  '';
}
