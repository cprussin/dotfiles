let
  sources = import ../../sources.nix;
in
  import ../../pkgs/dircolors-solarized/overlay.nix {
    src = sources.dircolors-solarized;
  }
