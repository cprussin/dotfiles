{ src }:

self: _: {
  zsh-git-prompt = self.callPackage ./derivation.nix { inherit src; };
}
