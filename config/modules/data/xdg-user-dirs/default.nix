{config, ...}: let
  home = config.primary-user.home;
in {
  primary-user.home-manager.xdg.userDirs = {
    enable = true;
    desktop = "${home}/";
    documents = "${home}/Scratch";
    download = "${home}/Scratch";
    music = "${home}/Scratch";
    pictures = "${home}/Camera";
    publicShare = "${home}/";
    templates = "${home}/Scratch";
    videos = "${home}/Camera";
  };
}
