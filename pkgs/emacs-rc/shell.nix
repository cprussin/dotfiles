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

    (setq git-path "${self.git}/bin/git"
          rg-path "${self.ripgrep}/bin/rg"
          browse-path "${self.chromium}/bin/chromium"
          msmtp-path "${self.msmtp}/bin/msmtp"
          shell-path "${self.stdenv.shell}"
          ispell-path "${self.ispell}/bin/ispell"
          mu-path "${self.mu}/bin/mu"
          editorconfig-path "${self.editorconfig-core-c}/bin/editorconfig"
          emoji-sets-path "${emoji-sets self}")

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
  } // (if prod then { } else {
    mu = super.mu.override { emacs = super.emacs; };
  });

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
