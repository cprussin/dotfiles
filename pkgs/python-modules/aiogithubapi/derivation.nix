{ buildPythonPackage, fetchPypi, aiohttp, backoff }:
let
  pname = "aiogithubapi";
  version = "21.8.1";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1iisb968dvw988k8q8b9lr3pvglb43qb6nyapggbpr7qpgs0yq4j";
  };

  propagatedBuildInputs = [
    aiohttp
    backoff
  ];
}
