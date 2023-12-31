{pkgs, ...}: {
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
    extraConfig = "load-module module-switch-on-connect";
  };

  primary-user.extraGroups = ["audio"];
}
