_: super: {
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope (
      epkgs: _: {
        emacs-rc = epkgs.callPackage ./derivation.nix {inherit epkgs;};
      }
    )
  );
}
