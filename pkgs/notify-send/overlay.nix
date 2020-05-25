{ src }:

self: _: {
  notify-send = self.callPackage ./derivation.nix { inherit src; };
}
