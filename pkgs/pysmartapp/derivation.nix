{ buildPythonPackage, fetchPypi, httpsig, tox, pytest }:
let
  pname = "pysmartapp";
  version = "0.3.2";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1i29aixpifvy3hcfkan1n7wc6w0lfizjv08mn3lkhgahf7s45wlh";
  };

  propagatedBuildInputs = [
    httpsig
    tox
    pytest
  ];
}
