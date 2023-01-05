self: super: {
  mako = self.callPackage ./derivation.nix {
    inherit (super) mako;
  };
}
