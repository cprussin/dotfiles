{ src }:

self: _: {
  dircolors-solarized = self.callPackage ./derivation.nix { inherit src; };
}
