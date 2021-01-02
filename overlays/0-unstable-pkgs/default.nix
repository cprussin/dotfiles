let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [ ];
    config = import ../../config/modules/system/nixpkgs/nixpkgs-config.nix;
  };
in
_: _: {
  inherit (pkgs-unstable) syncthing bitwig-studio home-assistant waybar makemkv;
}
