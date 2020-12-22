{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20201205162459";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "1r2br4vcs11wmg11a75980gacv2gilhdk58nk2fza7asgjffahaf";
  };
}
