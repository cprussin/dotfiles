{
  config,
  pkgs,
  ...
}: {
  nixpkgs.overlays = [
    (import ../../../../pkgs/emacs-rc/overlay.nix)
    (import ../../../../pkgs/emojione-png/overlay.nix)
    (import ../../../../pkgs/zoom-frm/overlay.nix {src = config.flake-inputs.zoom-frm;})
  ];
  primary-user.home-manager = {
    # No `services.emacs`: one daemon per user lands every frame on whichever
    # session it started in.  `emacsclient` starts one per display instead.
    programs.emacs = {
      enable = true;
      emacs-rc.enable = true;
    };

    # This session's daemon, up before the first frame asks for it.
    wayland.windowManager.sway.config.startup = [
      {command = "${pkgs.emacs}/bin/emacsclient -e t";}
    ];
  };
}
