{ buildPythonPackage, fetchPypi }:
let
  pname = "fnvhash";
  version = "0.1.0";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "13jsr2crcxv69n71p2mgyr4q7x5nwzvlkdmmna3kk7sg0l2xb0iy";
  };

  doCheck = false;
}
