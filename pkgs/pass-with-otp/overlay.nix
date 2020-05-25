self: super: {
  pass = self.callPackage ./derivation.nix {
    pass = super.pass;
  };
}
