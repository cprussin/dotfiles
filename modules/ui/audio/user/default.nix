{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
        volume = super.callPackage ./volume.nix {};
    })
  ];
}
