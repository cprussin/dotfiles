{ pkgs, lib, config, ... }:

{
  primary-user.home-manager.home = {
    packages = lib.mkForce [ pkgs.launcher ];

    file.".launcher-apps".source = pkgs.callPackage ./apps { inherit config; };

    sessionVariables = {
      EDITOR = "${pkgs.launcher}/bin/open";
      BROWSER = "${pkgs.launcher}/bin/browse";
    };
  };
}
