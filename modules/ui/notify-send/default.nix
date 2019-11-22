{ ... }:

{
  nixpkgs.overlays = [
    (
      self: _: {
        notify-send = self.callPackage ./derivation.nix {};
      }
    )
  ];
}
