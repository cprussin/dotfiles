{ stdenv, libXScrnSaver, makeWrapper, fetchurl, atomEnv, gtk2, at-spi2-atk }:

stdenv.mkDerivation rec {
  name = "minichrome-${version}";
  version = "0.0.2";

  src = fetchurl {
    url = "https://github.com/cprussin/minichrome/releases/download/v${version}/minichrome-linux-x64-${version}.tar.gz";
    sha256 = "1yggwmyxqqi7mg6419bp0grlqkgcd1ybn2m0hss7r6svryl48mh7";
  };

  buildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/{lib/minichrome,bin}
    mv * $out/lib/minichrome
    ln -s $out/lib/minichrome/minichrome $out/bin
  '';

  postFixup = ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${atomEnv.libPath}:${gtk2}/lib:${at-spi2-atk}/lib:$out/lib/minichrome" \
      $out/lib/minichrome/minichrome
    wrapProgram $out/lib/minichrome/minichrome \
      --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libXScrnSaver ]}/libXss.so.1
  '';
}
