{pkgs, ...}: {
  hardware.sane = {
    enable = true;
    extraBackends = [pkgs.sane-airscan pkgs.utsushi];
  };
  services.udev.packages = [pkgs.utsushi];
}
