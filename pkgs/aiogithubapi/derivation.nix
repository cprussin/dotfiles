{ callPackage, buildPythonPackage, fetchPypi, aiohttp }:

let
  backoff = callPackage ./backoff.nix {};
  pname = "aiogithubapi";
  version = "2.0.0";
in

buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1pv6wcplxfc16hlqmlfdl8xgskcz24cwf89f52wcxzjry0nfxily";
  };

  propagatedBuildInputs = [
    aiohttp
    backoff
  ];
}
