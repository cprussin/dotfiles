let
  sources = import ../../sources.nix;
in
import ../../pkgs/notify-send/overlay.nix { src = sources.notify-send; }
