{ pkgs, config, ... }:

let
  package = with pkgs; (callPackage ./package.nix { });
in

{
  home = {
    packages = [ package ];
    sessionVariables = {
      EDITOR = "open";
      APP_PATH = "${package}/share/apps";
      SECRETS=config.secrets;
    };
  };

  nixpkgs.overlays = [
    (self: super: {
      keepassxc = super.keepassxc.override {
        withKeePassNetworking = true;
      };
    })
  ];
}
