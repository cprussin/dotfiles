{src}: self: _: {
  mautrix-syncproxy = self.callPackage ./derivation.nix {inherit src;};
}
