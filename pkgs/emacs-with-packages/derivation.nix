{ callPackage, emacsWithPackages }:

emacsWithPackages (
  epkgs:
  callPackage ../../config/modules/ui/emacs/packages.nix {
    inherit epkgs;
  }
)
