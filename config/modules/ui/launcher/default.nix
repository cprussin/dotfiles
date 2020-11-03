{ pkgs, lib, config, ... }:
let
  launcher-apps = pkgs.callPackage ./apps { inherit config; };
  stripOverrides = pkgs.callPackage ../../../../lib/stripOverrides.nix { };
in
{
  primary-user.home-manager.home = {
    packages = lib.mkForce [ pkgs.launcher ];

    file.".launcher-apps".source = pkgs.linkFarm "launcher-apps" (
      lib.mapAttrsToList (name: path: { inherit name path; }) (
        stripOverrides launcher-apps
      )
    );

    sessionVariables = {
      EDITOR = "${pkgs.launcher}/bin/open";
      BROWSER = "${pkgs.launcher}/bin/browse";
    };
  };
}
