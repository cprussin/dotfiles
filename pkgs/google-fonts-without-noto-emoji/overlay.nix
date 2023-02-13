self: super: {
  google-fonts = self.callPackage ./derivation.nix {
    inherit (super) google-fonts;
  };
}
