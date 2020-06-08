let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [];
    config = import ../../config/modules/nix/nixpkgs/nixpkgs-config.nix;
  };
in

_: _: {
  inherit (pkgs-unstable) syncthing bitwig-studio;
}
