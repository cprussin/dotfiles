{ mako, gdk-pixbuf, librsvg, makeWrapper }:

mako.overrideAttrs (
  oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ makeWrapper gdk-pixbuf librsvg ];
    postInstall = ''
      ${oldAttrs.postInstall or ""}
      wrapProgram $out/bin/mako \
        --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
    '';
  }
)
