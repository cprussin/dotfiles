{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      backlight = super.callPackage ./backlight.nix {};
    })
  ];
}
