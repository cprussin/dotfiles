{ buildPythonPackage, fetchPypi, requests }:

let
  pname = "RachioPy";
  version = "1.0.3";
in

buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "15mmih35k3fq5mplq5439sx8l3gwdkc9r8fg55v7f885cf4q8ns6";
  };

  propagatedBuildInputs = [
    requests
  ];
}
