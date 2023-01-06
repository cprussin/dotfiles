let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [];
    config = import ../../config/modules/system/nix/nixpkgs-config.nix;
  };
  pkgs-master = import sources.nixpkgs-master {
    overlays = [];
    config = import ../../config/modules/system/nix/nixpkgs-config.nix;
  };
in
  _: _: {
    inherit (pkgs-unstable) syncthing bitwig-studio signald emacs;
    inherit (pkgs-master) makemkv;
  }
