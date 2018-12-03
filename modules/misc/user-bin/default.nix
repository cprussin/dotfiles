{ pkgs, config, ... }:

let
  package = pkgs.callPackage ./package.nix { };
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
}
