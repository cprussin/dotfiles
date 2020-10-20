{ buildPythonPackage
, fetchFromGitHub
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

  src = fetchFromGitHub {
    owner = "cprussin";
    repo = "python-ring-doorbell";
    rev = "master";
    sha256 = "0w4kly21zivnzl2ipvvijq7ba0bb53ncpc2r37n7i1zqmw1kjipj";
  };

  propagatedBuildInputs = [
    requests_oauthlib
    pytz
    pytest
    requests-mock
  ];
}
