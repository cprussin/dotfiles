{ fetchurl, bitwig-studio }:

bitwig-studio.overrideAttrs (
  _: rec {
    version = "3.1.3";
    name = "bitwig-studio-${version}";
    src = fetchurl {
      url = "https://downloads.bitwig.com/stable/${version}/bitwig-studio-${version}.deb";
      sha256 = "11z5flmp55ywgxyccj3pzhijhaggi42i2pvacg88kcpj0cin57vl";
    };
  }
)
