let
  nivSrc = fetchTarball {
    url = "https://github.com/nmattia/niv/tarball/1cf0ebaa7f3ae3139c6ea3941fff1bbcefe4b14f";
    sha256 = "0hy9w93qji61gmb5dg11njgcml4jybhv53y6ayny9cj8m1hvr9i0";
  };
  sources = import "${nivSrc}/nix/sources.nix" {
    sourcesFile = ./sources.json;
  };
  niv = import nivSrc {};
in

niv // sources
