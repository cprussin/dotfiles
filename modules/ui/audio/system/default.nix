{ pkgs, config, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };
  users.users.${config.primaryUserName}.extraGroups = [ "audio" ];
}
