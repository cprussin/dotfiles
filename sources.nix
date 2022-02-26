let
  nivSrc = fetchTarball {
    url = "https://github.com/nmattia/niv/tarball/5830a4dd348d77e39a0f3c4c762ff2663b602d4c";
    sha256 = "1d3lsrqvci4qz2hwjrcnd8h5vfkg8aypq3sjd4g3izbc8frwz5sm";
  };
  sources = import "${nivSrc}/nix/sources.nix" {
    sourcesFile = ./sources.json;
  };
  niv = import nivSrc {};
in
  niv // sources
