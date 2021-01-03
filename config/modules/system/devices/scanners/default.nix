{ pkgs, ... }:

{
  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.hplip ];
  };
}
