{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      launcher = super.callPackage ./launcher.nix {
        emacs = config.programs.emacs.finalPackage;
        secrets = config.secrets;
        terminal = config.terminal;
      };
    })
  ];

  home = {
    packages = [ pkgs.launcher ];
    sessionVariables.EDITOR = "${pkgs.launcher}/bin/open";
  };
}
