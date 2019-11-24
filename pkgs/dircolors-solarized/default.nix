{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "dircolors-solarized";
  src = fetchFromGitHub {
    owner = "seebi";
    repo = name;
    rev = "c8b07046acdfe4c07fa3bbafb0dd41327dc4e357";
    sha256 = "1a1sv5k9y96ryindaxx6w2ni3a1b6a5da8g03p9i5022f3fvq0ji";
  };
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    for file in $src/dircolors.*
    do
      install -m644 -D $file $out/etc/dircolors/solarized/''${file##*/}
        done
  '';
}
