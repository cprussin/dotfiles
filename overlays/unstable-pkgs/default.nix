let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [];
  };
in

_: _: {
  inherit (pkgs-unstable) syncthing bitwig-studio;
}
