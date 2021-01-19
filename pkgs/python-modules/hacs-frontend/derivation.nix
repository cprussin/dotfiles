{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20210103144316";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1ni4s5ig7k4h9309fjz5lk898fhg4g9zpz0qfivxzhkfyqrd9h4s";
  };
}
