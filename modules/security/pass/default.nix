{ config, ... }:

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
}
