{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20200522214300";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0wijw2h2kccgf06d9whdsfw25i7d0pvavxx7r7yrn1z1piyxacns";
  };
}
