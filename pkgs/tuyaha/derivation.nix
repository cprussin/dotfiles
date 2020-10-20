{ buildPythonPackage, fetchPypi, requests }:

let
  pname = "tuyaha";
  version = "0.0.8";
in

buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1pv9f3xak14cyxh5s1xl1h1cv240f3xfyl5cihbrv8rs6c051jk2";
  };

  propagatedBuildInputs = [
    requests
  ];
}
