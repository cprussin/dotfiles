{ pkgs, ... }:

{
  services.udev.extraRules = ''
    KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", RUN+="${pkgs.coreutils}/bin/touch /run/display-change"
  '';

  system.activationScripts.displayChange = ''
    umask 022
    touch /run/display-change
  '';
}
