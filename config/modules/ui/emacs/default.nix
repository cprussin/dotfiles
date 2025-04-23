{config, ...}: {
  nixpkgs.overlays = [
    (import ../../../../pkgs/emacs-rc/overlay.nix)
    (import ../../../../pkgs/emojione-png/overlay.nix)
    (import ../../../../pkgs/zoom-frm/overlay.nix {src = config.flake-inputs.zoom-frm;})
  ];
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc.enable = true;
    };
    services.emacs.enable = true;
  };
}
