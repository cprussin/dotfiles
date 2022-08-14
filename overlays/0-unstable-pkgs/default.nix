let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [];
    config = import ../../config/modules/system/nixpkgs/nixpkgs-config.nix;
  };
  pkgs-master = import sources.nixpkgs-master {
    overlays = [];
    config = import ../../config/modules/system/nixpkgs/nixpkgs-config.nix;
  };
in
  _: super: {
    inherit (pkgs-unstable) syncthing bitwig-studio signald;
    inherit (pkgs-master) makemkv;

    emacsPackagesFor = emacs: (
      (super.emacsPackagesFor emacs).overrideScope' (
        _: _: {
          inherit (pkgs-unstable.emacsPackagesFor emacs) org org-plus-contrib;
        }
      )
    );
  }
