{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      setup-monitors = super.callPackage ./setup-monitors.nix {};
    })
  ];
}
