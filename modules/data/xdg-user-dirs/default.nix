{ ... }:

{
  home.file.".config/user-dirs.dirs".text = ''
    XDG_DESKTOP_DIR="$HOME/"
    XDG_DOWNLOAD_DIR="$HOME/Scratch"
    XDG_TEMPLATES_DIR="$HOME/"
    XDG_PUBLICSHARE_DIR="$HOME/"
    XDG_DOCUMENTS_DIR="$HOME/Documents"
    XDG_MUSIC_DIR="$HOME/Music"
    XDG_PICTURES_DIR="$HOME/Camera"
    XDG_VIDEOS_DIR="$HOME/Camera"
  '';
}
