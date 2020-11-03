{ buildPythonPackage, fetchPypi, cryptography, zeroconf }:
let
  pname = "aiohomekit";
  version = "0.2.54";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0canwj6j12cal8gi8wgdf5h37w0gi08ylkv6mmj259iqqkz1ajpj";
  };

  propagatedBuildInputs = [
    cryptography
    zeroconf
  ];
}
