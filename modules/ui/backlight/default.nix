{ ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        backlight = super.callPackage ./backlight.nix {};
      }
    )
  ];
}
