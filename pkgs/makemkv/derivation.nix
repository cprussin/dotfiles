{
  lib,
  makeWrapper,
  symlinkJoin,
  makemkv,
  ccextractor,
}:
symlinkJoin {
  name = "makemkv";
  paths = [makemkv];
  buildInputs = [makeWrapper];
  postBuild = ''
    wrapProgram $out/bin/makemkv \
      --prefix PATH : ${lib.makeBinPath [ccextractor]}
  '';
}
