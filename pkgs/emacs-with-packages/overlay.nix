self: super: {
  mu = super.mu.override { emacs = super.emacs; };
  emacs = self.callPackage ./derivation.nix { };
}
