{ pkgs, config, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  users.users.${config.primaryUserName}.extraGroups = [ "audio" ];
}
