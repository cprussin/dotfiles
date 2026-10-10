{aptIndexes}: self: _: {
  chatgpt-desktop = self.callPackage ./derivation.nix {inherit aptIndexes;};
}
