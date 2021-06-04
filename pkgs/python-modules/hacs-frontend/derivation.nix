{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20210429001005";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0wmaa8mvjnl8qmb606ip43x5vvkjm2n767m312i2xdi876rc76g0";
  };
}
