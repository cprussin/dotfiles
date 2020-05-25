let
  sources = import ../../sources.nix;
in

{ nixpkgs ? sources.nixpkgs
, nix-linter ? sources.nix-linter
}:

  let
    pkgs = import nixpkgs {
      overlays = [
        (import ./overlay.nix { src = nix-linter; })
      ];
    };
  in

    pkgs.nix-linter
