{ trivialBuild }:

let
  sources = import ../../sources.nix;
in

trivialBuild {
  pname = "zoom-frm";
  src = sources.zoom-frm;
}
