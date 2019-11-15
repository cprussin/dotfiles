{ pkgs, config, lib, ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        get-aws-access-key = super.callPackage ./package.nix {};
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.home.packages = lib.mkForce [ pkgs.get-aws-access-key ];
}
