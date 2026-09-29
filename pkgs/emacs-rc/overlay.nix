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
      # A pgtk daemon puts every frame on the first Wayland display it
      # opened, so each display gets its own daemon, started on demand.
      postBuild = ''
        wrapProgram $out/bin/emacs \
          --prefix PATH : ${bin-path final}
        # `--prefix`: the daemon it starts is `execvp("emacs")`, off PATH.
        wrapProgram $out/bin/emacsclient \
          --prefix PATH : $out/bin \
          --run 'export EMACS_SOCKET_NAME="''${EMACS_SOCKET_NAME:-server-''${WAYLAND_DISPLAY:-tty}}"' \
          --run 'export ALTERNATE_EDITOR="''${ALTERNATE_EDITOR-}"'
      '';
    };
  }
