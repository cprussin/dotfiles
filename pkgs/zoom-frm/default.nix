{ epkgs }:

let
  sources = import ../../niv-sources.nix;
in

epkgs.trivialBuild {
  pname = "zoom-frm";
  src = sources.zoom-frm;
}
