let
  nivSrc = fetchTarball {
    url = "https://github.com/nmattia/niv/tarball/0ebb80e003c26d5388a9b74645fbdcfca3bdd0ef";
    sha256 = "0wpnk1n4vjyqwjjrm6dvkyh7xr7983rszfhfcg31v106qhfnh41c";
  };
  sources = import "${nivSrc}/nix/sources.nix" {
    sourcesFile = ./sources.json;
  };
  niv = import nivSrc {};
in
  niv // sources
