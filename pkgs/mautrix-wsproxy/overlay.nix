{ src }:

self: _: {
  mautrix-wsproxy = self.callPackage ./derivation.nix { inherit src; };
}
