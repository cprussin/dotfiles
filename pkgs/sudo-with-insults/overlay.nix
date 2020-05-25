self: super: {
  sudo = self.callPackage ./derivation.nix {
    sudo = super.sudo;
  };
}
