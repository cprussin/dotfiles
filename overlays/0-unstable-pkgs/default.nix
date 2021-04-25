let
  sources = import ../../sources.nix;
  pkgs-unstable = import sources.nixpkgs-unstable {
    overlays = [ ];
    config = import ../../config/modules/system/nixpkgs/nixpkgs-config.nix;
  };
in
_: super: {
  inherit (pkgs-unstable) syncthing bitwig-studio home-assistant waybar makemkv sway;

  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (
      _: _: {
        inherit (pkgs-unstable.emacsPackagesFor emacs) tide csv-mode;
      }
    )
  );
}
