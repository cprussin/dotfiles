{
  buildPythonPackage,
  fetchPypi,
}: let
  pname = "queueman";
  version = "0.5";
in
  buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "1myi5sy1hn6wjbgh16iamk2jhlrkscf2sd3nb1kz9wq7rsw2haff";
    };
  }
