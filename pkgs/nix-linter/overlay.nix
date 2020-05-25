{ src }:

self: _: {
  nix-linter = self.callPackage ./derivation.nix { inherit src; };
}
