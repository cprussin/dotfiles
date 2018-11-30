{ config, ... }:

{
  services.random-background = {
    enable = true;
    imageDirectory = toString ./. + "/bgimages";
  };
}
