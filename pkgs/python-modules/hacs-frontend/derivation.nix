{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20210822172723";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1q059f8clx28xzdwy8r46dmha817i1s7ispzbbb6n3g5bbfk9f7g";
  };
}
