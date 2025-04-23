let
  bin-path = pkgs:
    pkgs.lib.makeBinPath [
      pkgs.editorconfig-core-c
      pkgs.emojione-png
      pkgs.exiftool
      pkgs.git
      pkgs.hledger
      pkgs.imagemagick
      pkgs.ispell
      pkgs.libjpeg
      pkgs.optipng
      pkgs.pngcrush
      pkgs.pngnq
      pkgs.ripgrep
    ];
in
  final: _: {
    emacs = final.symlinkJoin {
      name = "emacs";
      paths = [
        ((final.emacsPackagesFor final.emacs-pgtk).withPackages (epkgs: [
          (epkgs.callPackage ./derivation.nix {inherit epkgs;})
        ]))
      ];
      buildInputs = [final.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/emacs \
          --prefix PATH : ${bin-path final}
      '';
    };
  }
