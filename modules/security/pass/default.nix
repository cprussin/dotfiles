{ pkgs, config, lib, ... }:

{
  nixpkgs.overlays = [
    (
      self: super: {
        pass = self.callPackage ./pass.nix {
          inherit config;
          pass = super.pass;
        };
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.home.packages = lib.mkForce [ pkgs.pass ];
}
