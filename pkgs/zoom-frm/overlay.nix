{src}: _: super: {
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope (
      epkgs: _: {
        zoom-frm = epkgs.callPackage ./derivation.nix {inherit src;};
      }
    )
  );
}
