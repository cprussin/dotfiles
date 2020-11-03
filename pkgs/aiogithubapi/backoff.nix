{ buildPythonPackage, fetchFromGitHub, pytestCheckHook, poetry, pytest-asyncio }:
let
  pname = "backoff";
  version = "1.10.0";
in
buildPythonPackage {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "litl";
    repo = pname;
    rev = "v${version}";
    sha256 = "1jj0l6pjx747d2yyvnzd3qbm4qr73sq6cc56dhvd8wqfbp5279x0";
  };

  format = "pyproject";

  nativeBuildInputs = [ poetry ];

  checkInputs = [ pytestCheckHook pytest-asyncio ];
}
