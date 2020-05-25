self: super: {
  mako = self.callPackage ./derivation.nix {
    mako = super.mako;
  };
}
