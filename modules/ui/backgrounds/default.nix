{ config, ... }:

{
  home-manager.users.${config.primaryUserName}.services.random-background = {
    enable = true;
    imageDirectory = toString ./. + "/bgimages";
  };
}
