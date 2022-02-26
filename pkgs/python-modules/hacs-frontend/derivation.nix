{
  buildPythonPackage,
  fetchPypi,
}: let
  pname = "hacs-frontend";
  version = "20211203065709";
in
  buildPythonPackage {
    inherit pname version;

    src = fetchPypi {
      inherit pname version;
      sha256 = "19qilld2ixn5mjwiyz8qr141p7g5m3q6hhl86114rzkxvbv1j6xq";
    };
  }
