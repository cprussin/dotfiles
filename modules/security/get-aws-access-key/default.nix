{ ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      get-aws-access-key = super.callPackage ./package.nix {};
    })
  ];
}
