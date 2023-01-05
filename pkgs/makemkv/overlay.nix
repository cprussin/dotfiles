self: super: {
  makemkv = self.callPackage ./derivation.nix {
    inherit (super) makemkv;
  };
}
