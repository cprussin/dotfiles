{ stdenv, libXScrnSaver, makeWrapper, fetchurl, atomEnv, gtk2, at-spi2-atk }:

stdenv.mkDerivation rec {
  name = "minichrome-${version}";
  version = "0.0.1";

  src = fetchurl {
    url = "https://github.com/cprussin/minichrome/releases/download/v0.0.1/minichrome-0.0.1.tar.gz";
    sha256 = "0ynjbhfvyjhi5zixvrni10f1l236ry387b9vsqbpm203p04391sf";
  };

  buildInputs = [ makeWrapper ];

  buildCommand = ''
    mkdir -p $out/{lib,bin}
    tar -xzf $src -C $out/lib
    mv $out/lib/minichrome-linux-x64 $out/lib/minichrome
    ln -s $out/lib/minichrome/minichrome $out/bin
    fixupPhase
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${atomEnv.libPath}:${gtk2}/lib:${at-spi2-atk}/lib:$out/lib/minichrome" \
      $out/lib/minichrome/minichrome
    wrapProgram $out/lib/minichrome/minichrome \
      --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libXScrnSaver ]}/libXss.so.1
  '';
}
