{ buildPythonPackage, fetchPypi, requests }:
let
  pname = "gTTS-token";
  version = "1.1.3";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1i8jq15ml5wmvms9walv7j23vw40nva1m4zgjx9j6gw1bfl1js4x";
  };

  propagatedBuildInputs = [
    requests
  ];
}
