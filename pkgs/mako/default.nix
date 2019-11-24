{ symlinkJoin, mako, gdk-pixbuf, librsvg, makeWrapper }:

symlinkJoin {
  name = "mako";
  paths = [ mako ];
  buildInputs = [ makeWrapper gdk-pixbuf librsvg ];
  postBuild = ''
    wrapProgram $out/bin/mako \
      --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
  '';
}
