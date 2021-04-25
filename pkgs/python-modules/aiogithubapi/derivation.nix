{ buildPythonPackage, fetchPypi, aiohttp, backoff }:
let
  pname = "aiogithubapi";
  version = "21.4.0";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1j1nlms5l9hzll3iq54ims40dlhfwccrxhrffb17a4pk3vygawg9";
  };

  propagatedBuildInputs = [
    aiohttp
    backoff
  ];
}
