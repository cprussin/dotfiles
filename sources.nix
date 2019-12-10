let
  nivSrc = fetchTarball {
    url = "https://github.com/nmattia/niv/tarball/6f03879b8a91fb0c33c7186da347b914deee3efc";
    sha256 = "04pl8b4gnbfakapjskm4030qlklwzjk0x45jn9gcnjpdl7s8ri33";
  };
  sources = import "${nivSrc}/nix/sources.nix" {
    sourcesFile = ./sources.json;
  };
  niv = import nivSrc {};
in

niv // sources
