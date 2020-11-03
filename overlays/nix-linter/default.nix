let
  sources = import ../../sources.nix;
in
import ../../pkgs/nix-linter/overlay.nix {
  src = sources.nix-linter;
}
