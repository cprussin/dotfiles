self: super: {
  sudo = self.callPackage ./derivation.nix {
    inherit (super) sudo;
  };
}
