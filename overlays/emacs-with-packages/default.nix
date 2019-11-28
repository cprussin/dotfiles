self: super: {
  mu = super.mu.override { emacs = super.emacs; };
  emacs = (self.emacsPackagesGen super.emacs).emacsWithPackages (
    epkgs: self.callPackage ../../config/modules/ui/emacs/packages.nix { inherit epkgs; }
  );
}
