{ epkgs }:

let
  sources = import ../../nix/sources.nix;
in

epkgs.trivialBuild {
  pname = "zoom-frm";
  src = sources.zoom-frm;
}
