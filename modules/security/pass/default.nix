{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      pass = self.callPackage ./pass.nix {
        pass = super.pass;
        secure = config.secure;
        wrapperDir = config.security.wrapperDir;
      };
    })
  ];
}
