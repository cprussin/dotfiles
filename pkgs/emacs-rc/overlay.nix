self: super: {
  emacs = self.emacsWithPackages (epkgs: [ epkgs.emacs-rc ]);

  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (
      epkgs: _: {
        emacs-rc = epkgs.callPackage ./derivation.nix { inherit epkgs; };
      }
    )
  );
}
