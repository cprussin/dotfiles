{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "202009091732";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0k67hy2d0v4w4ckyr6cclw9jj5zmxzzp3l3kh5g5yarfwlq5dag7";
  };
}
