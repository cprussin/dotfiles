{ nixpkgs }:

self: _: {
  esphome = self.callPackage ./derivation.nix { inherit nixpkgs; };
}
