{ prod ? false
, nixpkgs ? (import ../../sources.nix).nixpkgs
}:
let
  emoji-sets = self: self.linkFarm "emoji-sets" [
    {
      name = "emojione";
      path = pkgs.emojione-png;
    }
  ];

  entry-point = self: self.writeText "init.el" ''
    ${self.lib.optionalString (!prod) "(add-to-list 'load-path \"${toString ./.}\")"}

    (setq emacs-rc-git-path "${self.git}/bin/git"
          emacs-rc-rg-path "${self.ripgrep}/bin/rg"
          emacs-rc-browse-path "${self.chromium}/bin/chromium"
          emacs-rc-msmtp-path "${self.msmtp}/bin/msmtp"
          emacs-rc-shell-path "${self.stdenv.shell}"
          emacs-rc-ispell-path "${self.ispell}/bin/ispell"
          emacs-rc-editorconfig-path "${self.editorconfig-core-c}/bin/editorconfig"
          emacs-rc-emoji-sets-path "${emoji-sets self}")

    (require 'emacs-rc)
  '';

  emacs-packages = self: epkgs:
    self.callPackage ./dependencies.nix { inherit epkgs; };

  emacs-overlay = self: super: {
    emacs = self.symlinkJoin {
      name = "emacs";
      paths = if prod then [ super.emacs ] else [ (super.emacsWithPackages (emacs-packages self)) ];
      buildInputs = [ self.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/emacs \
          --add-flags '--no-init-file --load ${entry-point self}'
      '';
    };
  };

  pkgs = import nixpkgs {
    overlays = (if prod then [ (import ./overlay.nix) ] else [ ]) ++ [
      emacs-overlay
      (import ../../overlays/zoom-frm)
      (import ../../overlays/emojione-png)
    ];
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.emacs
  ];
}
