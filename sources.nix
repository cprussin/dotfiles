let
  nivSrc = fetchTarball {
    url = "https://github.com/nmattia/niv/tarball/914aba08a26cb10538b84d00d6cfb01c9776d80c";
    sha256 = "sha256:0gx316gc7prjay5b0cr13x4zc2pdbiwxkfkpjvrlb2rml80lm4pm";
  };
  sources = import "${nivSrc}/nix/sources.nix" {
    sourcesFile = ./sources.json;
  };
  niv = import nivSrc {};
in
  niv // sources
