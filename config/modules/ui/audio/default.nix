{ pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  services.jack.jackd.enable = true;

  primary-user.extraGroups = [ "audio" "jackaudio" ];
}
