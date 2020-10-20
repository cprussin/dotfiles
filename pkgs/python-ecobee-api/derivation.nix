{ buildPythonPackage, fetchPypi, requests }:

let
  pname = "python-ecobee-api";
  version = "0.2.7";
in

buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0ymrh1lkb2zzhnarnbcjafkym303fhvxqravypg37bjyrfyqx6cz";
  };

  propagatedBuildInputs = [
    requests
  ];
}
