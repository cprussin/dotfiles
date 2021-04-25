{ buildPythonPackage, fetchPypi }:
let
  pname = "hacs-frontend";
  version = "20210214181913";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0kn4dnbcyd33drqb361jmf9855xnmic7fnsxxd5z2yznvs0kv2ry";
  };
}
