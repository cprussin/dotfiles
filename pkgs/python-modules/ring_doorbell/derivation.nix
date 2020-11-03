{ buildPythonPackage
, fetchPypi
, requests_oauthlib
, pytz
, pytest
, requests-mock
}:
let
  pname = "ring_doorbell";
  version = "0.6.1";
in
buildPythonPackage {
  inherit pname version;

  src = fetchPypi {
    inherit pname version;
    sha256 = "0p29lda20257mrb7ziacf98219rnhx77wp0rs2psp4w3ajky69gz";
  };

  patchPhase = ''
    sed -i 's/requests==2.22.0/requests>=2.0.0/;s/requests-oauthlib==1.3.0/requests-oauthlib>=1.0.0/;s/oauthlib==3.1.0/oauthlib>=3.0.0/' \
           ./setup.py
  '';

  propagatedBuildInputs = [
    requests_oauthlib
    pytz
    pytest
    requests-mock
  ];
}
