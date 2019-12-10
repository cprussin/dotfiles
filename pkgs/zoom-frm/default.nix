{ epkgs }:

let
  sources = import ../../sources.nix;
in

epkgs.trivialBuild {
  pname = "zoom-frm";
  src = sources.zoom-frm;
}
