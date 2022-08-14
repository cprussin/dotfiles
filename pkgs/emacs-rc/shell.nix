{
  prod ? false,
  sources ? import ../../sources.nix,
  nixpkgs ? sources.nixpkgs,
  emacs-overlay ? sources.emacs-overlay,
  zoom-frm ? sources.zoom-frm,
}: let
  mkEmojiSets = emoji-pkgs:
    pkgs.linkFarm "emjoji-sets" (map
    (path: {
      inherit path;
      name = "${path.pname}-v${path.version}";
    })
    emoji-pkgs);

  entry-point =
    pkgs.writeText "init.el" ''
      ${pkgs.lib.optionalString (!prod) "(add-to-list 'load-path \"${toString ./.}\")"}

      (setq emacs-rc-git-path "${pkgs.git}/bin/git"
            emacs-rc-rg-path "${pkgs.ripgrep}/bin/rg"
            emacs-rc-browse-path "${pkgs.chromium}/bin/chromium"
            emacs-rc-shell-path "${pkgs.stdenv.shell}"
            emacs-rc-ispell-path "${pkgs.ispell}/bin/ispell"
            emacs-rc-editorconfig-path "${pkgs.editorconfig-core-c}/bin/editorconfig"
            emacs-rc-emoji-sets-path "${mkEmojiSets [pkgs.emojione-png]}")

      (require 'emacs-rc)
    '';

  emacs-packages = epkgs:
    if prod
    then [epkgs.emacs-rc]
    else pkgs.callPackage ./dependencies.nix {inherit epkgs;};

  emacs-pkg = pkgs.symlinkJoin {
    name = "emacs";
    paths = [((pkgs.emacsPackagesFor pkgs.emacsPgtkNativeComp).withPackages emacs-packages)];
    buildInputs = [pkgs.makeWrapper];
    postBuild = "wrapProgram $out/bin/emacs --add-flags '--no-init-file --load ${entry-point}'";
  };

  pkgs = import nixpkgs {
    overlays =
      [
        (import emacs-overlay)
      ]
      ++ (if prod
      then [(import ./overlay.nix)]
      else [])
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
