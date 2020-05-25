let
  sources = import ../../sources.nix;
in

{ nixpkgs ? sources.nixpkgs
, zoom-frm ? sources.zoom-frm
}:

  let
    pkgs = import nixpkgs {
      overlays = [
        (import ./overlay.nix { src = zoom-frm; })
      ];
    };
  in

    pkgs.emacsPackages.zoom-frm
