self: super: {
  mu = super.mu.override { emacs = super.emacs; };
  emacs = (self.emacsPackagesGen super.emacs).emacsWithPackages (
    epkgs: self.callPackage ../modules/ui/emacs/packages.nix { inherit epkgs; }
  );
}
