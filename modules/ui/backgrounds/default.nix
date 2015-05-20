{ config, ... }:

{
  services.random-background = {
    enable = true;
    imageDirectory = "${config.dotfiles}/modules/ui/backgrounds/bgimages";
  };
}
