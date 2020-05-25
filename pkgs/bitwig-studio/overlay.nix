self: super: {
  bitwig-studio = self.callPackage ./derivation.nix {
    bitwig-studio = super.bitwig-studio;
  };
}
