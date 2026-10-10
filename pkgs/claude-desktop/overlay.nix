{aptIndexes}: self: _: {
  claude-desktop = self.callPackage ./derivation.nix {inherit aptIndexes;};
}
