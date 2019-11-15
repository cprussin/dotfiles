{ pkgs, lib, config, ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        launcher = super.callPackage ./launcher.nix {
          inherit config;
        };
      }
    )
  ];

  home-manager.users.${config.primaryUserName}.home = {
    packages = lib.mkForce [ pkgs.launcher ];

    sessionVariables = {
      EDITOR = "${pkgs.launcher}/bin/open";
      BROWSER = "${pkgs.launcher}/bin/browse";
    };
  };
}
