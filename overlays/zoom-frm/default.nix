let
  sources = import ../../sources.nix;
in

import ../../pkgs/zoom-frm/overlay.nix { src = sources.zoom-frm; }
