{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      xlibs.xbacklight
      i3lock
    ];

    file.".xmonad/lib" = {
      source = ./lib;
      recursive = true;
    };
  };

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };
}
