{ buildPythonPackage
, fetchPypi
, casttube
, protobuf
, zeroconf
}:
let
  pname = "PyChromecast";
  version = "9.1.2";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "11afhbbzq8b4ivlg92zxapvbckzgg2arf5f87vlx0fbdlk75cxlh";
  };

  propagatedBuildInputs = [
    casttube
    protobuf
    zeroconf
  ];
}
