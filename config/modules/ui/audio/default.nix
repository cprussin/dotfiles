{ pkgs, lib, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  services.jack.jackd = {
    enable = true;
    session = lib.mkForce "";
  };

  primary-user.extraGroups = [ "audio" "jackaudio" ];
}
