_: super: {
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (
      epkgs: _: {
        zoom-frm = epkgs.callPackage ../pkgs/zoom-frm {
          inherit epkgs;
        };
      }
    )
  );
}
