{ ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        get-aws-access-key = super.callPackage ./package.nix {};
      }
    )
  ];
}
