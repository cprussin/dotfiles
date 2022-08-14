{pkgs, ...}: let
  sources = import ../../../../sources.nix;
in {
  nixpkgs.overlays = [
    (import sources.emacs-overlay)
    (import ../../../../pkgs/emacs-rc/overlay.nix)
    (import ../../../../pkgs/emojione-png/overlay.nix)
    (import ../../../../pkgs/zoom-frm/overlay.nix {src = sources.zoom-frm;})
  ];
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc = {
        enable = true;
        browse = "${pkgs.launcher}/bin/browse";
      };
    };
    services.emacs = {
      enable = true;
      package = (pkgs.emacsPackagesFor pkgs.emacsPgtkNativeComp).withPackages (epkgs: [epkgs.emacs-rc]);
    };
  };
}
