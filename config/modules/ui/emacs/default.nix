{config, ...}: let
  emacs-overlay = self: super: {
    emacs = (self.emacsPackagesFor super.emacs-pgtk).withPackages (epkgs: [epkgs.emacs-rc]);
  };
in {
  nixpkgs.overlays = [
    (import ../../../../pkgs/emacs-rc/overlay.nix)
    (import ../../../../pkgs/emojione-png/overlay.nix)
    (import ../../../../pkgs/zoom-frm/overlay.nix {src = config.flake-inputs.zoom-frm;})
    emacs-overlay
  ];
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc.enable = true;
    };
    services.emacs.enable = true;
  };
}
