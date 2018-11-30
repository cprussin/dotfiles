{ stdenv, fetchurl, nssTools, openssl }:

stdenv.mkDerivation rec {
  name = "metatron-cli-${version}";
  version = "1.189.0";

  propagatedBuildInputs = [
    nssTools
    openssl
  ];

  src = fetchurl {
    url = "https://mtrefresh.metatron.netflix.net/artifacts/cli/${version}/metatron-linux-amd64";
    sha256 = "0kzv4jy92a91y265s0nbvlb3n11ljqx0zlg367qng8zibflgkv7i";
  };

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/metatron.wrapped
    echo $(< $NIX_CC/nix-support/dynamic-linker) $out/bin/metatron.wrapped \"\$@\" > $out/bin/metatron
    chmod +x $out/bin/metatron{,.wrapped}
  '';
}
