{ pkgs, config, ... }:

let
  launcher = pkgs.callPackage ./launcher.nix {
    emacs = config.programs.emacs.finalPackage;
    secrets = config.secrets;
  };
in

{
  nixpkgs.overlays = [
    (self: super: { inherit launcher; })
  ];

  home = {
    sessionVariables.EDITOR = "${pkgs.launcher}/bin/open";
  };
}
