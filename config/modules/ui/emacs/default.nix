{pkgs, ...}: let
  sources = import ../../../../sources.nix;
  emacs-overlay = self: super: {
    emacs = (self.emacsPackagesFor super.emacs29-pgtk).withPackages (epkgs: [epkgs.emacs-rc]);
  };
in {
  nixpkgs.overlays = [
    (import ../../../../pkgs/emacs-rc/overlay.nix)
    (import ../../../../pkgs/emojione-png/overlay.nix)
    (import ../../../../pkgs/zoom-frm/overlay.nix {src = sources.zoom-frm;})
    emacs-overlay
  ];
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc = {
        enable = true;
        browse = "${pkgs.launcher}/bin/browse";
      };
    };
    services.emacs.enable = true;
  };
}
