{
  prod ? false,
  sources ? import ../../sources.nix,
  nixpkgs ? sources.nixpkgs,
  zoom-frm ? sources.zoom-frm,
}: let
  entry-point = pkgs.writeText "init.el" ''
    ${pkgs.lib.optionalString (!prod) "(add-to-list 'load-path \"${toString ./.}\")"}
    (setq emacs-rc-browse-path "${pkgs.chromium}/bin/chromium")
    (require 'emacs-rc)
  '';

  emacs-packages = epkgs:
    if prod
    then [epkgs.emacs-rc]
    else pkgs.callPackage ./dependencies.nix {inherit epkgs;};

  emacs-pkg = pkgs.symlinkJoin {
    name = "emacs";
    paths = [((pkgs.emacsPackagesFor pkgs.emacs-pgtk).withPackages emacs-packages)];
    buildInputs = [pkgs.makeWrapper];
    postBuild = "wrapProgram $out/bin/emacs --add-flags '--no-init-file --load ${entry-point}'";
  };

  pkgs = import nixpkgs {
    overlays =
      (
        if prod
        then [(import ./overlay.nix)]
        else []
      )
      ++ [
        (import ../emojione-png/overlay.nix)
        (import ../zoom-frm/overlay.nix {src = zoom-frm;})
      ];
  };
in
  pkgs.mkShell {
    buildInputs = [
      emacs-pkg
    ];
  }
