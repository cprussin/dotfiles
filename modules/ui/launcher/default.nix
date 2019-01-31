{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      launcher = super.callPackage ./launcher.nix {
        emacs = config.programs.emacs.finalPackage;
        terminal = config.terminal;
      };
    })
  ];
}
