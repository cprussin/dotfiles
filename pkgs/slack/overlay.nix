self: super: {
  slack = self.callPackage ./derivation.nix {
    slack = super.slack;
  };
}
