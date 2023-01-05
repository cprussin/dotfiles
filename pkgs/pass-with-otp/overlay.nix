self: super: {
  pass = self.callPackage ./derivation.nix {
    inherit (super) pass;
  };
}
